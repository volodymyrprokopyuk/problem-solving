package main

import (
  "fmt"
  "time"
  "sync"
  "context"
)

func gorClosureLoop() {
  var wg sync.WaitGroup
  for _, i := range []int{1, 2, 3} {
    wg.Add(1)
    go func() {
      defer wg.Done()
      fmt.Println(i) // 3, 3, 3
    }()
    go func(i int) {
      defer wg.Done()
      fmt.Println(i) // 2, 1, 3
    }(i) // parametrize gor closure inside a loop
    j := i // capture local variable per iteration
    go func() {
      defer wg.Done()
      fmt.Println(j) // 2, 1, 3
    }()
  }
  wg.Wait()
}

func waitGroup() {
  // wg is a concurrency-safe counter
  var wg sync.WaitGroup // making a zero value useful
  // must be outside of a tracking gor
  wg.Add(1) // increment a gor counter
  go func() {
    defer wg.Done() // decrement a gor counter
    time.Sleep(200 * time.Millisecond)
    fmt.Println("a")
  }()
  wg.Add(1)
  go func() {
    defer wg.Done()
    time.Sleep(100 * time.Millisecond)
    fmt.Println("b")
  }()
  wg.Wait() // block gor until a counter == 0
  fmt.Println("done")
}

func rwMutex() {
  var value int
  var mu sync.RWMutex
  writeShared := func (val int) {
    mu.Lock() // a single writer can hold a write lock, no readers
    defer mu.Unlock()
    value = val
  }
  readShared := func() int {
    mu.RLock() // multiple readers can hold a read lock, no writer
    defer mu.RUnlock()
    return value
  }
  var wg sync.WaitGroup
  wg.Add(2)
  go func() {
    defer wg.Done()
    writeShared(1)
  }()
  go func() {
    defer wg.Done()
    time.Sleep(1 * time.Millisecond)
    fmt.Println(readShared()) // 1
  }()
  wg.Wait()
}

func condBroadcast() {
  balance := 0
  cond := sync.NewCond(&sync.Mutex{})
  listen := func(goal int) {
    cond.L.Lock()
    defer cond.L.Unlock()
    // critical section 1: wait for a condition
    for balance < goal { // exit a loop when a condition is met
      // listen for an update. Must be within a critical section
      cond.Wait() // .L.Unlock => wait for the next broadcast => .L.Lock
    }
    // critical section 2: a condition is met
    fmt.Println("goal", balance)
  }
  go listen(3)
  go listen(5)
  for i := 0; i < 7; i++ { // producer
    time.Sleep(100 * time.Millisecond)
    cond.L.Lock()
    balance++
    cond.L.Unlock()
    cond.Broadcast() // broadcast an update to all listeners
  }
}

func funcOnce() {
  var count int
  inc := func() {
    count++
  }
  var once sync.Once
  var wg sync.WaitGroup
  for i := 0; i < 3; i++ {
    wg.Add(1)
    go func() {
      defer wg.Done()
      // only one call to inc will succeed even from different gors
      once.Do(inc)
    }()
  }
  wg.Wait()
  fmt.Println(count)
}

func objectPool() {
  var count int
  // concurrency-safe pool of objects
  // minimizes object creation through reuse
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
    case <- done: // wait for a gor
      fmt.Println("done")
      return
    default: // immediately executed if all cases are blocked
      time.Sleep(20 * time.Millisecond)
      fmt.Println("working...")
    }
  }
}

func cancelGor() {
  var wg sync.WaitGroup
  done := make(chan struct{}) // done is never written to (only closed)
  in := make(chan int)
  for i := 0; i < 3; i++ {
    wg.Add(1)
    go func(i int) {
      defer wg.Done()
      for {
        select {
        case <- done: // receive a zero value when close(done)
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
      // consider an error as a possible outcome
      if i < 0 {
        ch <- Outcome{fmt.Errorf("oh"), 0}
        continue
      }
      ch <- Outcome{nil, i}
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
  out := make(chan int)
  go func() {
    defer wg.Done()
    defer close(out)
    for i := start; i < end; i++ {
      time.Sleep(100 * time.Millisecond)
      select { // ensures preemptible generator
      case <- done: // waits for a cancellation signal
        fmt.Println("generator done")
        return
      case out <- i:
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
      select { // ensures preemptible transformer
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

func gorParallel() {
  maxGors := 5
  ch := make(chan int)
  res := make(chan int, maxGors) // buffered out chan
  var wg sync.WaitGroup
  wg.Add(maxGors) // must be in a waiting parent gor
  for i := 0; i < maxGors; i++ {
    go func() { // parallel maxGors
      defer wg.Done()
      res <- 10 * <- ch // maxGors non-blocking writes
    }()
  }
  // input
  for i := 0; i < maxGors; i++ {
    ch <- i
  }
  close(ch)
  // output
  wg.Wait()
  close(res)
  for i := range res {
    fmt.Println(i)
  }
}

func rateLimiter() {
  limit := 3
  bucket := make(chan struct{}, limit) // buffered bucket
  for i := 0; i < limit; i++ {
    bucket <- struct{}{} // fill a bucket with tokens
  }
  var wg sync.WaitGroup
  wg.Add(limit)
  for i := 0; i < 5; i++ { // execute rate limited tasks
    select {
    case <- bucket: // take a token from a bucket
      go func(i int) { // start a task
        defer wg.Done()
        time.Sleep(100 * time.Millisecond)
        fmt.Println(i)
        bucket <- struct{}{} // return a token to a bucket
      }(i)
    default: // bucket is empty
      fmt.Println("rate limited", i)
    }
  }
  wg.Wait() // wait for passed tasks to complete
}

func ctxCancelTimeout() {
  task := func(ctx context.Context, wg *sync.WaitGroup) {
    defer wg.Done()
    for {
      select {
      // a channel is closed when a context is cancelled
      case <- ctx.Done(): // immediately returns a zero value when closed
        if ctx.Err() == context.Canceled {
          fmt.Println("canceled")
        }
        if ctx.Err() == context.DeadlineExceeded {
          fmt.Println("timeout")
        }
        return
      default:
        fmt.Println("working...")
        time.Sleep(100 * time.Millisecond)
      }
    }
  }
  // cancel context
  ctx, cancel := context.WithCancel(context.Background())
  // once created a cancellable context must be cancelled
  defer cancel()
  var wg sync.WaitGroup
  wg.Add(1)
  go task(ctx, &wg)
  time.Sleep(300 * time.Millisecond)
  cancel() // further cancellations are ignored
  wg.Wait()
  // timeout context
  wg.Add(1)
  ctx, cancel2 := context.WithTimeout(context.Background(), 300 * time.Millisecond)
  defer cancel2()
  go task(ctx, &wg)
  wg.Wait()
}

func workerPool() {
  n := 4
  var wg sync.WaitGroup
  wg.Add(n)
  ch := make(chan int, n) // buffered channel
  for i := 0; i < n; i++ { // start n workers
    go func(i int) {
      defer wg.Done()
      for v := range ch { // keep workers running
        fmt.Printf("%v: %v\n", i, v)
        time.Sleep(100 * time.Millisecond)
      }
    }(i)
  }
  for i := 0; i < 2 * n; i++ {
    ch <- i // distribute tasks between n workers
  }
  close(ch) // stop n workers
  wg.Wait()
}

func gracefulTermination() {
  ch := make(chan int, 2)
  done := make(chan struct{})
  var wg sync.WaitGroup
  wg.Add(3)
  go func() {
    defer wg.Done()
    for _, i := range []int{1, 2} {
      ch <- i // producer
    }
    close(done) // signal termination
  }()
  go func() {
    defer wg.Done()
    for _, i := range []int{3, 4, 5, 6} {
      ch <- i // producer
    }
  }()
  go func() {
    defer wg.Done()
    for {
      select {
      case v := <- ch: // regular consumption
        time.Sleep(500 * time.Millisecond)
        fmt.Println(v) // 1, 2
      case <- done:
        for {
          select {
          case v := <- ch: // graceful consumption
            time.Sleep(500 * time.Millisecond)
            fmt.Println("graceful", v) // 3, 4, 5, 6
          default:
            fmt.Println("done")
            return // graceful termination
          }
        }
      }
    }
  }()
  wg.Wait()
}

func mergeChannels() {
  ch1, ch2 := make(chan int), make(chan int)
  mg := make(chan int)
  go func() {
    for _, i := range []int{1, 2, 3} {
      ch1 <- i // producer
    }
    close(ch1)
  }()
  go func() {
    for _, i := range []int{4, 5, 6} {
      ch2 <- i // producer
    }
    close(ch2)
  }()
  go func() {
    for ch1 != nil || ch2 != nil { // at least one channel is open
      time.Sleep(100 * time.Millisecond)
      select {
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
  for v := range mg {
    fmt.Println(v)
  }
}

type Counter struct {
  // a counter interface does not expose any concurrency primitives
  mu sync.Mutex
  Value int
}

func (c *Counter) Inc(val int) {
  c.mu.Lock()
  defer c.mu.Unlock()
  c.Value += val // thread safe, atomic counter
}

func atomicCounter() {
  var wg sync.WaitGroup
  var c Counter
  for i := 0; i < 5; i++ {
    wg.Add(1)
    go func() {
      defer wg.Done()
      c.Inc(1)
    }()
  }
  wg.Wait()
  fmt.Println(c.Value)
}

func main() {
  // gorClosureLoop()
  // waitGroup()
  // rwMutex()
  // condBroadcast()
  // funcOnce()
  // objectPool()
  // rangeCloseChan()
  // timeout()
  // workWhileWaiting()
  // cancelGor()
  // errorHandling()
  pipeline()
}
