import std/json

type
  Message* = object
    userName*, text*: string

proc parseMessage*(rawMessage: string): Message =
  let jsonMessage = parseJson(rawMessage)
  result.userName = jsonMessage["userName"].getStr
  result.text = jsonMessage["text"].getStr

func createMessage*(userName, text: string): string =
  let jsonMessage = %*{"userName": userName, "text": text}
  $jsonMessage & "\c\l"

when isMainModule: # no new scope, just code inclusion
  block parseMessageSuccess: # explicit new scope
    let
      rawMessage = """{"userName": "Vlad", "text": "Hi"}"""
      jsonMessage = parseMessage(rawMessage)
    doAssert jsonMessage.userName == "Vlad"
    doAssert jsonMessage.text == "Hi"

  block parseMessageFailure:
    let rawMessage = "invalid JSON"
    try:
      discard parseMessage(rawMessage)
      doAssert false, "exception not raised"
    except JsonParsingError:
      doAssert true

  block createMessageSuccess:
    let expectedMessage = """{"userName":"Vlad","text":"Hi"}""" & "\c\l"
    doAssert createMessage("Vlad", "Hi") == expectedMessage
  echo "SUCCESS: all tests passed"
