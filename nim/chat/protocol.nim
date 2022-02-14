import std/json

type
  Message* = object
    userName*, message*: string

func newMessage*(userName, message: string): Message =
  Message(userName: userName, message: message)

proc parseMessage*(rawMessage: string): Message =
  let jsonMessage = parseJson rawMessage
  result.userName = jsonMessage["userName"].getStr
  result.message = jsonMessage["message"].getStr

func createMessage*(userName, message: string): string =
  let jsonMessage = %*{"userName": userName, "message": message}
  $jsonMessage & "\c\l"

when isMainModule: # no new scope, just code inclusion
  block parseMessageSuccess: # explicit new scope
    let
      rawMessage = """{"userName": "Vlad", "message": "Hi"}"""
      jsonMessage = parseMessage rawMessage
    doAssert jsonMessage.userName == "Vlad"
    doAssert jsonMessage.message == "Hi"
  block parseMessageFailure:
    let rawMessage = "invalid JSON"
    try:
      discard parseMessage rawMessage
      doAssert false, "exception not raised"
    except JsonParsingError:
      doAssert true
  block createMessageSuccess:
    let expectedMessage = """{"userName":"Vlad","message":"Hi"}""" & "\c\l"
    doAssert createMessage("Vlad", "Hi") == expectedMessage
  echo "SUCCESS: all tests passed"
