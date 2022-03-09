import std/[json, options]

let
  rawContent = """{
    "aString": "Abc",
    "anInteger": 1,
    "aFloat": 1.2,
    "aBool": true,
    "aNull": null,
    "anArray": ["Abc", 1, 1.2, true, null],
    "anObject": {"aType": "object", "aContent": 1}
  }"""
  # Dynamic nested structure of JsonNodes object variants
  j = parseJson rawContent

# [] operator raises an exception if the field does not exist
echo j["aString"].getStr
echo j["anInteger"].getInt
echo j["aFloat"].getFloat
echo j["aBool"].getBool
echo j["aNull"]
echo j["anArray"][1].getInt
echo j["anObject"]["aContent"].getInt
# {} returns nil (does not throw an exception) if the field does not exist
echo j{"invalid"}.getInt
# Default value for getter if the field is null or does not exist
echo j["aNull"].getStr("default")
echo pretty j
for k, v in j: echo k, " ", v # JObject iterator

type
  User = object
    name: string
    id: Option[int] # Use Option for optional JSON fields

let
  rawUser = """{"name": "Vlad", "id": 1}"""
  jsonUser = parseJson rawUser
# Unmarshal dynamic JsonNode to an object type or a tuple
echo jsonUser.to User
echo jsonUser.to tuple[name: string, id: Option[int]]

# Serializaiton to JSON
echo %*[{"name": jsonUser["name"].getStr, "id": jsonUser["id"].getInt + 1}]

let ja = %*[1, 2, 3, 4, 5]
echo ja[1..^2] # Slice JSON array
for e in ja: echo e.getInt # JArray iterator

var jo = %*{"a": {"b": {"c": 1}}}
echo jo{"a", "b", "c"}.getInt # Traverse JSON object
jo{"a", "b", "c"} = 2.newJInt # Create new node
echo jo
jo{"a", "b", "c"} = %*{"d": 3}
echo jo
