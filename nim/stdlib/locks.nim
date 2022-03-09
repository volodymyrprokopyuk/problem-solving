import std/[threadpool, locks]

var
  counterLock: Lock
  glCounter {.guard: counterLock.} = 0 # guard global shared variable

proc lockIncExplicit(until: int) =
  for _ in 0..<until:
    counterLock.acquire # explicity acquire / release + {.locks: [...]}
    {.locks: [counterLock].}:
      try:
        var counter = glCounter
        counter.inc
        glCounter = counter
      finally:
        counterLock.release

proc lockIncImplicit(until: int) =
  for _ in 0..<until:
    withLock counterLock: # implicit acquire / release lock
      var counter = glCounter
      counter.inc
      glCounter = counter

counterLock.initLock
spawn lockIncExplicit(60_000)
spawn lockIncImplicit(60_000)
sync()
echo glCounter
