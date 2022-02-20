import std/[os, threadpool, asyncdispatch, asyncnet], protocol

proc parseOptions(): tuple[serverAddr, userName: string] =
  if paramCount() != 2: quit "Usage: ./client <server> <user>"
  (serverAddr: paramStr(1), userName: paramStr(2))

proc processMessages(userName: string, socket: AsyncSocket) =
  # Execute a blocking operation in a new thread
  var flMessage = spawn stdin.readLine
  while true:
    if flMessage.isReady:
      let
        line = ^flMessage # wait for the result of a blocking operation
        message = createMessage(userName, line)
      asyncCheck socket.send(message) # run code in the background
      flMessage = spawn stdin.readLine
    poll() # call the event loop manually

proc connect(socket: AsyncSocket, serverAddr: string, port = 7687) {.async.} =
  await socket.connect(serverAddr, port.Port) # pause and poll
  echo "Connected to ", serverAddr
  while true:
    let
      line = await socket.recvLine # pause and poll
      message = parseMessage(line)
    echo message.userName, " said ", message.text

let
  (serverAddr, userName) = parseOptions()
  socket = newAsyncSocket()
asyncCheck connect(socket, serverAddr) # run code in the backgound
processMessages(userName, socket)
