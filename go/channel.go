package main

import (
  "fmt"
  "time"
  "sync"
  "context"
)

func gorLoop() {
  // parametrize gor closure inside a loop
  ch := make(chan int)
  slc := []int{1, 2, 3}
  for _, e := range slc {
    go func() { ch <- e }() // 3, 3, 3
    go func(e int) { ch <- e }(e) // 2, 1, 3
  }
  for range slc {
    fmt.Println(<- ch)
  }
}

func generator(start, end int) <-chan int {
  ch := make(chan int)
  go func() {
    for i := start; i < end; i++ {
      ch <- i
    }
    close(ch)
  }()
  return ch
}

func gorGenerator() {
  // on break or return => gor leak = gor write blocks indefinitely
  for i := range generator(1, 4) {
    fmt.Println(i) // 1, 2, 3
  }
}

func makeSleep(dur time.Duration) func() int64 {
  return func() int64 {
    time.Sleep(dur)
    return dur.Milliseconds()
  }
}

func doneChannel() {
  sleeps := []func() int64{
    makeSleep(200 * time.Millisecond),
    makeSleep(100 * time.Millisecond),
  }
  ch := make(chan int64) // collects the first result
  done := make(chan struct{}) // is never written to (only closed)
  for _, sleep := range sleeps {
    go func(sleep func() int64) { // parameterized gor
      select {
      case ch <- sleep(): // multiple gors write to the same chan
      case <- done: // slower gors exit when done is closed
      }
    }(sleep)
  }
  res := <- ch // only the first gor succeeds
  close(done) // signals exit to slower gors (by reading a zero value)
  fmt.Println(res) // 100
}

func generatorCancel(start, end int) (<-chan int, func()) {
  ch := make(chan int)
  done := make(chan struct{})
  go func() {
    for i := start; i < end; i++ {
      select {
      case ch <- i:
      case <- done:
        close(ch)
        return
      }
    }
    close(done)
    close(ch)
  }()
  // let a caller to exit early from a loop by calling cancel()
  return ch, func() { close(done) }
}

func gorGeneratorCancel() {
  ch, cancel := generatorCancel(1, 5)
  for i := range ch {
    if i == 3 {
      cancel() // signal early exit from a loop = no gor leaking
      break
    }
    fmt.Println(i) // 1, 2
  }
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

func timeOut() {
  done := make(chan struct{})
  go func() {
    time.Sleep(50 * time.Millisecond)
    fmt.Println("done")
    close(done) // signal success
  }()
  select {
  case <- done:
  case <- time.After(100 * time.Millisecond): // trigger timeout
    fmt.Println("timeout")
  }
}

func waitGroup() {
  var wg sync.WaitGroup // making zero value useful
  wg.Add(2) // set gor counter
  go func() {
    defer wg.Done() // decrement gor counter
    time.Sleep(200 * time.Millisecond)
    fmt.Println("a")
  }()
  go func() {
    defer wg.Done()
    time.Sleep(100 * time.Millisecond)
    fmt.Println("b")
  }()
  wg.Wait() // block gor until a counter == 0
  fmt.Println("done")
}

var sharedValue = 1
var m sync.RWMutex

func readShared() int {
  m.RLock()
  defer m.RUnlock()
  return sharedValue
}

func writeShared(value int) {
  m.Lock()
  defer m.Unlock()
  sharedValue = value
}

func gorMutex() {
  var wg sync.WaitGroup
  wg.Add(2)
  go func() {
    defer wg.Done()
    writeShared(2)
  }()
  go func() {
    defer wg.Done()
    time.Sleep(10 * time.Millisecond)
    fmt.Println(readShared())
  }()
  wg.Wait()
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

func condBroadcast() {
  balance := 0
  cond := sync.NewCond(&sync.Mutex{})
  listen := func(goal int) {
    cond.L.Lock()
    defer cond.L.Unlock()
    // critical section 1
    for balance < goal { // exit a loop when a condition is met
      // listen for update. Must be within a critical section
      cond.Wait() // Unlock => wait for next broadcast => Lock
    }
    // critical section 2
    fmt.Println("goal", balance)
  }
  go listen(3)
  go listen(5)
  for i := 0; i < 7; i++ {
    time.Sleep(100 * time.Millisecond)
    cond.L.Lock()
    balance++
    cond.L.Unlock()
    cond.Broadcast() // broadcast an update to all listeners
  }
}

func main() {
  condBroadcast()
}
