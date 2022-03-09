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

type
  Box = object # object variant on bool
    case empty: bool # empty field is always present
    of false:
      content: string
    else:
      discard

# let box = Box(empty: false, content: "full")
# echo box

import std/[tables, hashes]

type
  Cat = object
    name: string

func newCat(name: string): Cat = Cat(name: name)

func hash(cat: Cat): Hash = # Cat hash
  result = cat.name.hash
  result = !$result # finalize hash

# let catOwners = toTable({
#   newCat("Orysia"): "Lana", newCat("Kotsyub"): "Vlad"
# })
# echo catOwners
# echo catOwners[newCat("Orysia")]

# var catOwners = initTable[Cat, string]()
# catOwners[newCat("Orysia")] = "Lana"
# catOwners[newCat("Kotsyub")] = "Vlad"
# echo catOwners
# echo catOwners[newCat("Kotsyub")]

import std/sets

# let family = toHashSet(["Vlad", "Lana"])
# echo "Vlad" in family

import std/algorithm

# var numbers = [3, 2, 8, 5, 7, 6, 1, 4, 9, 0]
# echo numbers.sorted(Descending) # sorted sequence copy
# numbers.sort(cmp[int]) # in-place sorting
# echo numbers

import std/osproc

# let (version, _) = execCmdEx "uname -sr"
# echo version

# let
#   param = "--port=1234"
#   pname = param.substr(2, 5)
#   pvalue = param[7..^1]
#   pcomp = param.split({'=', ':'})
# echo pname, " ", pvalue
# echo pcomp[0].substr(2), " ", pcomp[1]

import std/[asyncdispatch, httpclient]

# let
#   client = newAsyncHttpClient()
#   res = waitFor client.get("https://nim-lang.org/")
# echo res.status
# echo waitFor res.body

type
  ThreadData = (string, string)

# var
#   globalData = "Global data"
#   globalData2 = "Global data 2"

proc showGlobalData(tdata: ThreadData) {.thread.} =
  let (data, data2) = tdata
  echo data
  echo data2

# var thread: Thread[ThreadData]
# createThread[ThreadData](thread, showGlobalData, (globalData, globalData2))
# joinThread(thread) # no return value from the thread procedure

proc threadCount(n: int) {.thread.} =
  for i in 0..n: stdout.write(i)
  echo()

# var ths: array[2, Thread[int]]
# createThread(ths[0], threadCount, 9)
# createThread(ths[1], threadCount, 9)
# joinThreads(ths)

import std/[threadpool, os]

proc forwardCount(n: int): int =
  for i in 0..n: stdout.write(i)
  echo()
  n

# let
#   r1 = spawn forwardCount(9)
#   r2 = spawn forwardCount(9)
# sync() # waits for all spawned procedures to finish
# echo ^r1, " ", ^r2 # retrieves result from the thread procedure

proc raiseException(): string {.raises: [ValueError].} =
  raise newException(ValueError, "oh")

# let flowVarLine = spawn raiseException() #stdin.readLine()
# while not flowVarLine.isReady: # periodically check result from the thread
#   echo "Waiting for input"
#   sleep(3000)
# echo ^flowVarLine

var globalCounter = 0

proc increment(until: int) =
  for i in 0..<until: # globalCounter.inc # works correctly
    var counter = globalCounter
    counter.inc
    globalCounter = counter

# spawn increment(60_000)
# spawn increment(60_000)
# sync()
# echo globalCounter

import std/locks

var
  counterLock: Lock
  globalCounter2 {.guard: counterLock.} = 0 # variable guarded with lock
# counterLock.initLock

proc lockIncrement(until: int) =
  for i in 0..<until:
    withLock counterLock: # acquire / release lock to access guarded variable
      var counter = globalCounter2
      counter.inc
      globalCounter2 = counter

# spawn lockIncrement(60_000)
# spawn lockIncrement(60_000)
# sync()
# echo globalCounter2

var greetChannel: Channel[string] # channel as global variable

proc channelGreet(name: string) =
  sleep(1000)
  greetChannel.send("Hello " & name)

# greetChannel.open
# spawn channelGreet("Vladyslava")
# echo greetChannel.recv() # blocks main thread until the message is received

var counterChannel: Channel[int]

proc channelIncrement(until: int) =
  var counter = 0
  for _ in 0..<until: counter.inc # local increment
  counterChannel.send(counter)

# counterChannel.open
# spawn channelIncrement(60_000)
# spawn channelIncrement(60_000)
# sync()
# var globalCounter3 = 0
# globalCounter3.inc(counterChannel.recv)
# globalCounter3.inc(counterChannel.recv)
# echo globalCounter3
