import std/[strutils, os]
import pkg/[nimja/parser, nimja/nimjautils]

type
  User = object
    name: string
    id: int

func newUser(name: string, id: int): User = User(name: name, id: id)

proc renderTemplate(users: openArray[User], access = true): string =
  # Render generic template with spacific `block`s (reuslt &= fragment)
  compileTemplateFile(getScriptDir() / "nimja-specific.nwt")

const
  users = [newUser("Vlad", 1), newUser("Lana", 2), newUser("Lada", 3)]

echo renderTemplate(users)

iterator renderRange(slice: Slice): string =
  # Render and yield chunks of the template (yield fragment)
  compileTemplateStr(
    "{% for i in slice %}<span>{{i}}</span>{% endfor %}", iter = true)

# for chunk in renderRange(0..5): echo chunk
