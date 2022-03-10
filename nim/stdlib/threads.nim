import std/[os, threadpool]

# createThread, joinThreads = strictly parallel (uses multicore CPU)
# spawn, ^ = strictly parallel (uses multicore CPU)
# {.async.}, await = strictly concurrent (uses single-threaded event loop)

# threads = long-running tasks that communicate with other threads periodically
# Each thread has its own GC and heap and sharing of memory is restricted
# To use the most of CPU power create one thread per CPU core
# Unhandlad exception in any thread terminates the whole process

var th1, th2: Thread[Slice[int]] # Slice input param for the thread procedure

proc count(slice: Slice[int]) {.thread.} = # thread procedure, no return value
  # thread-local finish handler
  onThreadDestruction(proc() = echo "Finished ", slice)
  for i in slice:
    echo i
    sleep(500)

th1.createThread(count, 0..4) # deterministic thread execution, full control
th2.createThread(count, 5..9) # starts thread execution with procedure + param
echo th1.running, " ", th2.running # checks thread running status
joinThreads(th1, th2) # waits for all threads to finish


# threadpool = short-living tasks that produce a result (no communication with
# other threads)

proc countAndResult(slice: Slice[int]): int = # thread procedure + return value
  for i in slice:
    echo i
    sleep(500)
  slice.b

let
  # Enqueues a task + param to the thread pool (non-deterministic execution)
  # Always spawns a new task. The task is never execute in the calling thread
  result1: FlowVar[int] = spawn countAndResult(0..4) # data flow variable
  result2: FlowVar[int] = spawn countAndResult(5..9)

# Alternative 1
sync() # waits for all spawned tasks (optional)
echo ^result1, " ", ^result2 # blocks and retrievs FlowVar result

# Alternative 2
while not result1.isReady: # checks task result readiness
  echo "Waiting"
  sleep(1000)
echo "Result ", ^result1
# blocks and passes the result
result2.awaitAndThen(proc(b: int) = echo "Result ", b)
