import std/[os, times]
import pkg/nimja/parser
import dbmodel

proc renderLogin*(title = "Tweet - Login"): string =
  compileTemplateFile(getScriptDir() / "uilogin.nwt")

proc renderTimeline*(
  user: User, messages: seq[Message], principal: string,
  title = "Tweet - Timeline"): string =
  compileTemplateFile(getScriptDir() / "uitimeline.nwt")
