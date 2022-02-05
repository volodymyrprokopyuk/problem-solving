import std/[strutils, sequtils, sugar]

# let names = ["A word", "A noun", "Countable noun"]

# Procedural style
# for name in names: echo (name.split(" ")[0], name.split(" ")[1])

# Functional style
# echo names.map((s: string) -> (string, string) =>
#   (s.split(" ")[0], s.split(" ")[1]))
# echo names.mapIt (it.split(" ")[0], it.split(" ")[1])
