import std/os

var greetChannel: Channel[string] # channel in module level global variable

proc greet(name: string) =
  sleep(500)
  greetChannel.send("Hello " & name)

greetChannel.open
try:
  var greetThread: Thread[string] # channel for inter thread communication
  greetThread.createThread(greet, "Vladyslava")
  echo greetChannel.recv() # blocking read from a channel
  greetThread.joinThread

  greetThread.createThread(greet, "Lada")
  while true:
    let (available, message) = greetChannel.tryRecv # non-blocking read
    if available:
      echo message
      break;
    sleep(100)
    stdout.write(".") # doing other work while waiting for a message
  greetThread.joinThread
finally:
  greetChannel.close
