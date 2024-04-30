package main

import (
	"context"
	"fmt"
	"math/rand"
	"os"
	"os/signal"
	"sync"
	"time"
)

func waitGroup() {
  var wg sync.WaitGroup // make a zero value useful
  wg.Add(1) // increment a counter
  go func() {
    defer wg.Done() // decrement a counter
    time.Sleep(200 * time.Millisecond)
    fmt.Println("a")
  }()
  wg.Add(1)
  go func() {
    defer wg.Done()
    time.Sleep(100 * time.Millisecond)
    fmt.Println("b")
  }()
  wg.Wait() // block a gor until a counter == 0
  fmt.Println("done")
}

func counterMutex() {
  n := 100000
  var cnt int
  var mtx sync.Mutex // make a zero value useful
  var wg sync.WaitGroup
  wg.Add(2)
  go func() {
    defer wg.Done()
    for range n {
      mtx.Lock() // a single writer can hold a write lock, no readers
      cnt++ // write critical section
      mtx.Unlock()
    }
  }()
  go func() {
    defer wg.Done()
    for range n {
      mtx.Lock()
      cnt-- // read critical section
      mtx.Unlock()
    }
  }()
  wg.Wait()
  fmt.Println(cnt)
}

// read-preferring readers-writer mutex
type RRWMutex struct {
  readers int
  readMtx sync.Mutex
  writeMtx sync.Mutex
}

func (m *RRWMutex) Lock() {
  m.writeMtx.Lock()
}

func (m *RRWMutex) Unlock() {
  m.writeMtx.Unlock()
}

func (m *RRWMutex) RLock() {
  m.readMtx.Lock()
  m.readers++
  if m.readers == 1 {
    m.writeMtx.Lock()
  }
  m.readMtx.Unlock()
}

func (m *RRWMutex) RUnlock() {
  m.readMtx.Lock()
  m.readers--
  if m.readers == 0 {
    m.writeMtx.Unlock()
  }
  m.readMtx.Unlock()
}

// write-preferring readers-writer mutex
type RWWMutex struct {
  readers int
  waitingWriters int
  writerActive bool
  cnd *sync.Cond
}

func NewRWWMutex() *RWWMutex {
  return &RWWMutex{cnd: sync.NewCond(new(sync.Mutex))}
}

func (m *RWWMutex) Lock() {
  m.cnd.L.Lock()
  m.waitingWriters++
  for m.readers > 0 || m.writerActive {
    m.cnd.Wait()
  }
  m.waitingWriters--
  m.writerActive = true
  m.cnd.L.Unlock()
}

func (m *RWWMutex) Unlock() {
  m.cnd.L.Lock()
  m.writerActive = false
  m.cnd.Broadcast()
  m.cnd.L.Unlock()
}

func (m *RWWMutex) RLock() {
  m.cnd.L.Lock()
  for m.waitingWriters > 0 || m.writerActive {
    m.cnd.Wait()
  }
  m.readers++
  m.cnd.L.Unlock()
}

func (m *RWWMutex) RUnlock() {
  m.cnd.L.Lock()
  m.readers--
  if m.readers == 0 {
    m.cnd.Broadcast()
  }
  m.cnd.L.Unlock()
}

func readWriteMutex() {
  var wg sync.WaitGroup
  // var mtx sync.Mutex // 200ms
  // var mtx sync.RWMutex // 115ms
  // var mtx RRWMutex // 115ms
  mtx := NewRWWMutex() // 115ms
  reader := func() {
    defer wg.Done()
    mtx.RLock()
    time.Sleep(10 * time.Millisecond)
    mtx.RUnlock()
  }
  writer := func() {
    defer wg.Done()
    mtx.Lock()
    time.Sleep(10 * time.Millisecond)
    mtx.Unlock()
  }
  start := time.Now()
  for range 10 {
    wg.Add(2)
    go reader()
    go writer()
  }
  wg.Wait()
  fmt.Println(time.Since(start))
}

func condBroadcast() {
  var balance int
  cnd := sync.NewCond(new(sync.Mutex))
  listen := func(goal int) {
    cnd.L.Lock()
    // critical section 1: wait for a condition
    for balance < goal { // exit a loop when a condition is met
      // listen for an update. Must be within a critical section
      // cnd.L.Unlock => wait for the next cnd.Broadcast => cnd.L.Lock
      cnd.Wait()
    }
    // critical section 2: a condition is met
    fmt.Printf("goal %v\n", balance)
    cnd.L.Unlock()
  }
  go listen(3)
  go listen(5)
  for i := 0; i < 7; i++ { // producer
    time.Sleep(100 * time.Millisecond)
    cnd.L.Lock()
    balance++ // critical section 2: update a shared state
    cnd.Broadcast() // broadcast an update to all listeners
    cnd.L.Unlock()
  }
}

func allJoined() {
  n := 4
  var wg sync.WaitGroup
  cnd := sync.NewCond(new(sync.Mutex))
  var cnt int
  for i := range n {
    wg.Add(1)
    go func() {
      defer wg.Done()
      time.Sleep(time.Duration(rand.Intn(300)) * time.Millisecond)
      cnd.L.Lock()
      cnt++
      fmt.Printf("%v joined\n", i)
      if cnt == n {
        cnd.Broadcast()
      }
      for cnt < n {
        cnd.Wait()
      }
      cnd.L.Unlock()
      fmt.Printf("%v all joined: %v\n", i, cnt)
    }()
  }
  wg.Wait()
}

type Semaphore struct {
  permits int
  cnd *sync.Cond
}

func NewSemaphore(n int) *Semaphore {
  return &Semaphore{n, sync.NewCond(new(sync.Mutex))}
}

func (s *Semaphore) Acquire() {
  s.cnd.L.Lock()
  for s.permits <= 0 {
    s.cnd.Wait()
  }
  s.permits--
  s.cnd.L.Unlock()
}

func (s *Semaphore) Release() {
  s.cnd.L.Lock()
  s.permits++
  if s.permits > 0 {
    s.cnd.Signal()
  }
  s.cnd.L.Unlock()
}

func contextCancelTimeout() {
  var wg sync.WaitGroup
  task := func(ctx context.Context) {
    defer wg.Done()
    for {
      select {
      // a channel is closed when a context is canceled
      case <- ctx.Done(): // immediately returns a zero value when closed
        switch ctx.Err() {
        case context.Canceled:
          fmt.Println("canceled")
        case context.DeadlineExceeded:
          fmt.Println("timeout")
        }
        return
      default: // non-blocking
        fmt.Println("working...")
        time.Sleep(100 * time.Millisecond)
      }
    }
  }
  // cancel context
  ctx, cancel := context.WithCancel(context.Background())
  // once created a cancellable context must be canceled
  defer cancel()
  wg.Add(1)
  go task(ctx)
  time.Sleep(300 * time.Millisecond)
  cancel() // further cancellations are ignored
  wg.Wait()
  // timeout context
  ctx, cancel2 := context.WithTimeout(
    context.Background(), 300 * time.Millisecond,
  )
  defer cancel2()
  wg.Add(1)
  go task(ctx)
  wg.Wait()
}

func gorClosureInsideLoop() {
  var wg sync.WaitGroup
  for _, i := range []int{1, 2, 3} {
    wg.Add(1)
    go func() {
      defer wg.Done()
      fmt.Println(i) // 3, 3, 3
    }()
    // parametrize gor closure inside a loop
    go func(i int) {
      defer wg.Done()
      fmt.Println(i) // 2, 1, 3
    }(i)
    // capture value in a local variable per iteration
    i := i
    go func() {
      defer wg.Done()
      fmt.Println(i) // 2, 1, 3
    }()
  }
  wg.Wait()
}

func funcOnce() {
  var count int
  inc := func() {
    count++
  }
  var wg sync.WaitGroup
  var once sync.Once
  for i := 0; i < 3; i++ {
    wg.Add(1)
    go func() {
      defer wg.Done()
      // only one call to inc will succeed even from different gors
      once.Do(inc)
    }()
  }
  wg.Wait()
  fmt.Println(count) // 1
}

func objectPool() {
  var count int
  // concurrency-safe pool of objects that
  // minimizes object creation through reuse and
  // warms up object cache using pool.Put()
  pool := sync.Pool{
    New: func() any {
      count++
      return count // creates a new object
    },
  }
  var wg sync.WaitGroup
  for i := 0; i < 10; i++ {
    wg.Add(1)
    go func() {
      defer wg.Done()
      obj := pool.Get() // reuse if available, create new otherwise
      defer pool.Put(obj) // recycle a used object
      fmt.Println(obj)
    }()
  }
  wg.Wait()
  fmt.Println("total", count)
}

func rangeCloseChan() {
  ch := make(chan int)
  go func() {
    // a closed chan immediately returns zero values
    // a closed chan can unblock multiple gors
    defer close(ch)
    for i := 0; i < 10; i++ {
      time.Sleep(100 * time.Millisecond)
      ch <- i
    }
  }()
  var wg sync.WaitGroup
  wg.Add(2)
  go func() {
    defer wg.Done()
    for i := range ch { // receive from a chan until it is closed
      fmt.Println("gor 1:", i)
    }
  }()
  go func() {
    defer wg.Done()
    for i := range ch {
      fmt.Println("gor 2:", i)
    }
  }()
  wg.Wait()
}

func timeout() {
  done := make(chan struct{})
  go func() {
    time.Sleep(50 * time.Millisecond)
    close(done) // signal success
  }()
  select {
  case <- done:
    fmt.Println("done")
  // chan time.Time is closed after a timeout expires
  case <- time.After(100 * time.Millisecond): // trigger timeout
    fmt.Println("timeout")
  }
}

func workWhileWaiting() {
  done := make(chan struct{})
  go func() {
    time.Sleep(100 * time.Millisecond)
    close(done)
  }()
  for {
    select {
    case <- done: // blocks until done
      fmt.Println("done")
      return
    default: // immediately executed if all cases are blocked
      time.Sleep(20 * time.Millisecond)
      fmt.Println("working...")
    }
  }
}

func cancelAllGors() {
  var wg sync.WaitGroup
  done := make(chan struct{}) // done is never written to (only closed)
  in := make(chan int)
  for i := 0; i < 3; i++ {
    wg.Add(1)
    go func(i int) {
      defer wg.Done()
      for {
        select {
        case <- done: // close(done) cancels all listening gors
          fmt.Printf("%v: done\n", i)
          return
        case v := <- in:
          fmt.Printf("%v: %v\n", i, v)
        }
      }
    }(i)
  }
  for i := 0; i < 10; i++ {
    time.Sleep(100 * time.Millisecond)
    in <- i
  }
  close(done) // signal gor cancellation
  wg.Wait()
  close(in)
}

// an outcome is either an error or a result
type Outcome struct {
  Error error
  Result int
}

func errorHandling() {
  ch := make(chan Outcome)
  go func() {
    for _, i := range []int{1, 2, -1, 3, -1, 4, 5} {
      time.Sleep(100 * time.Millisecond)
      // do not handle an error locally
      // consider an error as a possible valid outcome
      if i < 0 {
        ch <- Outcome{fmt.Errorf("oh"), 0} // error outcome
        continue
      }
      ch <- Outcome{nil, i} // result outcome
    }
    close(ch)
  }()
  for out := range ch {
    // handle error at a parent where more context is available
    if out.Error != nil {
      fmt.Println(out.Error)
      continue
    }
    fmt.Println(out.Result)
  }
}

func generator(
  wg *sync.WaitGroup, done <-chan struct{}, start, end int,
) <-chan int {
  wg.Add(1)
  // channel ownership
  out := make(chan int) // create a channel
  go func() {
    defer wg.Done()
    defer close(out) // close a channel
    for i := start; i < end; i++ {
      time.Sleep(100 * time.Millisecond)
      select { // ensures preemption
      case <- done: // waits for a cancellation signal
        fmt.Println("generator done")
        return
      case out <- i: // send to a channel
      }
    }
  }()
  return out
}

func transform(
  wg *sync.WaitGroup, done <-chan struct{}, in <-chan int, tr func(int) int,
) <-chan int {
  wg.Add(1)
  out := make(chan int)
  go func() {
    defer wg.Done()
    defer close(out)
    for v := range in {
      select { // ensures preemption
      case <- done: // waits for a cancellation signal
        fmt.Println("transform done")
        return
      case out <- tr(v):
      }
    }
    fmt.Println("transform in closed")
  }()
  return out
}

func pipeline() {
  var wg sync.WaitGroup
  done := make(chan struct{})
  // each pipeline stage performs concurrently
  in := generator(&wg, done, 0, 5)
  in2 := transform(&wg, done, in, func(i int) int { return i + 1 })
  in3 := transform(&wg, done, in2, func(i int) int { return i * 10 })
  for v := range in3 {
    fmt.Println(v)
    if v == 20 {
      break
    }
  }
  close(done) // signal all stages to exit early (after break)
  wg.Wait()
}

func fanIn(
  wg *sync.WaitGroup, done <-chan struct{}, ins ...<-chan int,
) <-chan int {
  wg.Add(1)
  var inWg sync.WaitGroup
  out := make(chan int)
  for _, in := range ins {
    inWg.Add(1)
    go func(in <-chan int) {
      defer inWg.Done()
      // receives from each input channel
      for {
        select {
        case <- done:
          return
        case v, open := <- in:
          if !open {
            return
          }
          out <- v
        }
      }

    }(in)
  }
  go func() {
    defer wg.Done()
    inWg.Wait()
    fmt.Println("closing fanin")
    // closes a common output channel
    // after all input channels have been processed
    close(out)
  }()
  return out
}

func fanOutIn() {
  var wg sync.WaitGroup
  done := make(chan struct{})
  in := generator(&wg, done, 0, 10)
  n := 3
  ins := make([]<-chan int, 0, n)
  // fan out
  for i := 0; i < n; i++ {
    // start parallel processing from the in channel
    in2 := transform(&wg, done, in, func(i int) int {
      time.Sleep(500 * time.Millisecond)
      return i + 1
    })
    ins = append(ins, in2) // collect fan out input channels
  }
  in3 := fanIn(&wg, done, ins...) // combine fan out input channels
  for v := range in3 {
    fmt.Println(v)
    if v == 5 {
      break
    }
  }
  close(done)
  wg.Wait()
}

func tee(done <-chan struct {}, in <-chan int) (<-chan int, <-chan int) {
  out1, out2 := make(chan int), make(chan int)
  go func() {
    defer close(out1)
    defer close(out2)
    for v := range in {
      // per iteration channel copies to assign nil
      var o1, o2 = out1, out2
      for i := 0; i < 2; i++ {
        select {
        case <- done:
          return
        case o1 <- v:
          o1 = nil // blocks forever, lets send v to o2
        case o2 <- v:
          o2 = nil // blocks forever, lets send v to o1
        }
      }
    }
  }()
  return out1, out2
}

func teeChan() {
  var wg sync.WaitGroup
  done := make(chan struct{})
  defer close(done)
  in := generator(&wg, done, 0, 5)
  in1, in2 := tee(done, in)
  wg.Add(2)
  go func() {
    defer wg.Done()
    for v := range in1 {
      fmt.Println("1: ", v)
    }
  }()
  go func() {
    defer wg.Done()
    for v := range in2 {
      fmt.Println("2: ", v)
    }
  }()
  wg.Wait()
}

func heartbeat() {
  var wg sync.WaitGroup
  done := make(chan struct{})
  defer close(done)
  in := generator(&wg, done, 0, 5)
  out := make(chan int)
  tick := time.NewTicker(100 * time.Millisecond)
  hb := make(chan struct{})
  wg.Add(1)
  go func() {
    defer wg.Done()
    defer close(hb)
    defer close(out)
    for {
      select {
      case <- done: // cancellation
        fmt.Println("done")
        return
      case v, open := <- in:
        if !open {
          fmt.Println("in closed")
          return
        }
        out <- v // processing
      case <- tick.C:
        select {
        case hb <- struct{}{}: // heartbeat
        default: // do not block if hb is not read
        }
      }
    }
  }()
  timeout := 90 * time.Millisecond
  timer := time.NewTimer(timeout)
  defer timer.Stop()
  main: for {
    timer.Reset(timeout)
    select {
    case <- hb:
      fmt.Println("heatrbeat")
    case <- timer.C:
      fmt.Println("timeout: neither value nor heartbeat")
    case v, open := <- out:
      if !open {
        break main
      }
      fmt.Println(v)
    }
  }
  wg.Wait()
}

func workerPool() {
  n := 10 // tasks
  limit := 3 // worker pool
  var wg sync.WaitGroup
  done := make(chan struct{})
  in := generator(&wg, done, 0, n)
  // buffered channel for parallel processing
  out := make(chan int, limit)
  for i := 0; i < limit; i++ {
    wg.Add(1)
    go func(i int) { // parallel processing
      defer wg.Done()
      for {
        select {
        case <- done:
          fmt.Printf("%v: cancelled\n", i)
          return
        case v, open := <- in:
          if !open {
            fmt.Printf("%v: in closed\n", i)
            return
          }
          fmt.Printf("%v: %v\n", i, v)
          out <- v * 10
        }
      }
    }(i)
  }
  // collect results of parallel processing
  for i := 0; i < n; i++ {
    v := <- out
    fmt.Println(v)
    if v == 50 {
      break
    }
  }
  close(done) // after all n tasks or after a break
  wg.Wait()
  close(out) // channel ownership
}

func rateLimiter() {
  limit := 3
  bucket := make(chan struct{}, limit) // buffered bucket
  for i := 0; i < limit; i++ {
    bucket <- struct{}{} // fill a bucket with tokens
  }
  var wg sync.WaitGroup
  for i := 0; i < 5; i++ { // execute rate limited tasks
    select {
    case <- bucket: // take a token from a bucket
      wg.Add(1)
      go func(i int) { // start a task
        defer wg.Done()
        defer func () {
          bucket <- struct{}{} // return a token to a bucket
        }()
        time.Sleep(100 * time.Millisecond)
        fmt.Println(i)
      }(i)
    default: // bucket is empty
      fmt.Println("rate limited", i)
    }
  }
  wg.Wait() // wait for passed tasks to complete
}

func gracefulTermination() {
  done := make(chan struct{})
  ch := make(chan int, 2)
  go func() {
    for _, i := range []int{1, 2} {
      time.Sleep(100 * time.Millisecond)
      ch <- i
    }
    fmt.Println("graceful start")
    close(done) // signal early termination
  }()
  go func() {
    for _, i := range []int{3, 4, 5, 6} {
      time.Sleep(100 * time.Millisecond)
      ch <- i
    }
    close(ch)
  }()
  for {
    select {
    case v := <- ch: // regular consumption
      fmt.Println(v)
    case <- done:
      for v := range ch { // graceful consumption
        fmt.Println("graceful", v)
      }
      fmt.Println("graceful end")
      return // graceful termination
    }
  }
}

func mergeChannels() {
  ch1, ch2 := make(chan int), make(chan int)
  mg := make(chan int)
  go func() {
    for _, i := range []int{1, 2, 3} {
      time.Sleep(100 * time.Millisecond)
      ch1 <- i
    }
    close(ch1)
  }()
  go func() {
    for _, i := range []int{4, 5, 6} {
      time.Sleep(100 * time.Millisecond)
      ch2 <- i
    }
    close(ch2)
  }()
  go func() {
    for ch1 != nil || ch2 != nil { // at least one channel is open
      select {
      // use a nil channel to disable a case in select
      // after a channel has been closed
      case v, open := <- ch1:
        if !open {
          ch1 = nil // block forever = remove case from select
          break
        }
        mg <- v // merge
      case v, open := <- ch2:
        if !open {
          ch2 = nil // block forever = remove case from select
          break
        }
        mg <- v // merge
      }
    }
    close(mg)
  }()
  for v := range mg { // consume merged values
    fmt.Println(v)
  }
}

type Counter struct {
  mu sync.Mutex
  Value int
}

func (c *Counter) Inc(val int) {
  c.mu.Lock()
  defer c.mu.Unlock()
  c.Value += val // thread-safe, atomic counter
}

func atomicCounter() {
  var wg sync.WaitGroup
  var c Counter
  for i := 0; i < 5; i++ {
    wg.Add(1)
    go func() { // concurrent update of a counter
      defer wg.Done()
      // a counter interface is concurrency-free
      c.Inc(1)
    }()
  }
  wg.Wait()
  fmt.Println(c.Value)
}

func processSignal() {
  ch := make(chan os.Signal)
  signal.Notify(ch, os.Interrupt)
  go func() {
    sig := <- ch
    fmt.Printf("\nsignal: %v\n", sig)
    os.Exit(1)
  }()
  var str string
  fmt.Scanln(&str)
  fmt.Println(str)
}

func main() {
  // waitGroup()
  // counterMutex()
  readWriteMutex()
  // condBroadcast()
  // allJoined()
  // contextCancelTimeout()

  // gorClosureInsideLoop()
  // funcOnce()
  // objectPool()
  // rangeCloseChan()
  // timeout()
  // workWhileWaiting()
  // cancelAllGors()
  // errorHandling()
  // pipeline()
  // fanOutIn()
  // teeChan()
  // heartbeat()
  // workerPool()
  // rateLimiter()
  // gracefulTermination()
  // mergeChannels()
  // atomicCounter()
  // processSignal()
}
