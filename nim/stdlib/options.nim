import std/options

proc find(haystack: string, needle: char): Option[int] =
  for i, c in haystack:
    if c == needle: return some(i) # create some value (present)
  none(int) # create empty value of type (absent)

let found = "Vlad".find 'A'
if found.isSome: echo found.get # check for some and get value
if found.isNone: echo found.get(-1) # check for none and return default value

import pkg/fusion/matching

{.experimental: "caseStmtMacros".}

case found: # pattern matching
of Some(@pos): echo pos
of None(): echo found
