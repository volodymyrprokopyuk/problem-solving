{.warning[UnusedImport]: off.}
{.hint[XDeclaredButNotUsed]: off.}

import std/[strutils, sequtils, sugar]

# let names = ["A word", "A noun", "Countable noun"]

# Procedural style
# for name in names: echo (name.split(" ")[0], name.split(" ")[1])

# Functional style
# echo names.map((s: string) -> (string, string) =>
#   (s.split(" ")[0], s.split(" ")[1]))
# echo names.mapIt (it.split(" ")[0], it.split(" ")[1])

# let s = "Володимир"
# echo s, " ", s.toUpperAscii, " ", s.toUpper

func incInt(a: int): int = # compile-time execution by NimVM
  debugEcho "compile-time"
  a + 1

# const c = incInt 1
# echo c

# let
#   змінна = "Працює" # Unicode identifier
#   `var` = "stropping (keyword as identifier)"
# echo змінна, " ", `var`

# let
#   a = [1, 2, 3, 4, 5, 6, 7]
#   b = a.filter(proc(x: int): bool = x mod 2 != 0) # anonymous proc
#   c = a.filter((x: int) -> bool => x mod 2 != 0) # typed arrow proc
#   d = a.filter(x => x mod 2 != 0) # inferred arrow proc
#   e = a.filterIt(it mod 2 != 0) # it template as param
#   f = a.filterIt: it mod 2 != 0 # it template as block
# echo b, " ", c, " ", d, " ", e, " ", f

type
  IntPredicate = proc(x: int): bool # proc type
  IntPredicate2 = (x: int) -> bool # proc type sugar

# let
#   always: IntPredicate = proc(x: int): bool = true
#   never: IntPredicate2 = proc(x: int): bool = false
# echo always 1, " ", never 0

type Direction = enum dNorth, dEast, dSouth, dWest # ordinal type

# let
#   a = [dNorth: "North", dEast: "East"] # array indexed with enum
#   s = {dEast, dWest} # set with enum base type
# echo a[dNorth], " ", dWest in s

type
  Phone = tuple[brand: string] # named fields
  Tablet = (string) # anonymous fields

# let
#   phone = (brand: "Xiaomi")
#   tablet = ("Xiaomi",)
#   (tBrand) = tablet # tuple unpacking
# echo phone == tablet # structural typing
# echo phone.brand, " ", tablet[0], " ", tBrand

type
  Person = object
    name: string

# var
#   p: Person # stack allocated + default fields
#   rp: ref Person # heap allocated + nil
# echo p, " ", rp.repr

proc setName(person: var Person, name: string) =
  person.name = name # var mutable object

proc setName(person: ref Person, name: string) =
  person.name = name # ref pass by reference

# var
#   p = Person(name: "Vlad") # stack allocated
#   rp: ref Person = new Person # heap allocated
# echo p
# p.setName "Volodymyr"
# echo p
# echo rp[]
# rp.name = "Lana"
# echo rp[]
# rp.setName "Svitlana"
# echo rp[]
