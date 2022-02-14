import std/[strutils, os, threadpool], protocol

proc parseOptions(): tuple[serverAddr, userName: string] =
  if paramCount() != 2: quit "Usage: ./client <server> <user>"
  (serverAddr: paramStr 1, userName: paramStr 2)

proc readMessage(prompt: string = "> "): string =
  stdout.write prompt
  stdin.readLine.strip

proc processMessages(userName: string) =
  while true:
    let
      # Execute a blocking operation in a new thread
      flMessage = spawn readMessage()
      # Wait for the result of the blocking operation
      message = ^flMessage
    if message == "q": break
    let pMessage = newMessage(userName, message)
    echo pMessage

let (serverAddr, userName) = parseOptions()
processMessages userName
