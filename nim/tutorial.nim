{.hint[XDeclaredButNotUsed]: off.}
{.warning[UnusedImport]: off.}

proc greet() =
  stdout.write "> "
  let name = stdin.readLine
  echo "Hello ", name

# greet()

from std/strutils import parseInt

proc readNumber() =
  stdout.write "> "
  let number = parseInt readLine stdin
  case number
  of 0..3, 7..9: echo "outer"
  else: echo "inner"

# readNumber()

proc factorial(n: int): int =
  if n < 2: 1
  else: n * factorial n - 1

# echo factorial 4

proc sumTillNegative(a: varargs[int]): int =
  for e in a: (if e < 0: return; result += e)

# echo sumTillNegative()
# echo sumTillNegative(1, 2, 3)
# echo sumTillNegative(1, 2, -3)

proc printSeq(s: seq, n: int = -1) =
  let n = if n == -1: s.len else: min(n, s.len)
  for i in 0..<n: echo s[i]

# printSeq @[1, 2, 3, 4, 5, 6], 3

proc quotientRemainder(x, y: int; quotient, remainder: var int) =
  quotient = x div y; remainder = x mod y

# var x, y: int
# quotientRemainder(7, 3, x, y)
# echo x, " ", y

proc quotientRemainder2(x = 1, y = 1): (int, int) = (x div y, x mod y)

# echo quotientRemainder2(7, 3)
# echo quotientRemainder2(y = 3, x = 7)
# echo quotientRemainder2()

proc toString(x: int): string =
  if x > 0: "positive" elif x < 0: "negative" else: "zero"

proc toString(x: bool): string =
  if x: "yes" else: "no"

# echo toString 1
# echo toString -1
# echo toString 0
# echo toString true
# echo toString false

proc `\`(x: int): string = r"\op{" & $x & "}" # custom operator

# echo \1
# echo `\`(2)
# echo 3.`\`

proc even(x: int): bool # forward declaration

proc odd(x: int): bool =
  assert(x >= 0)
  if x == 0: false else: x == 1 or even(x - 1)

proc even(x: int): bool =
  assert(x >= 0)
  if x == 1: false else: x == 0 or odd(x - 1)

# echo even 6, odd 7

iterator tickup(a, b: int): int =
  assert a <= b
  var i = a
  while i <= b: yield i; inc i

# for i in tickup(4, 9): echo i
# for i in tickup(4, 4): echo i

proc paramSize(x: openarray[int]): int = len x

# echo paramSize [1, 2, 3]
# echo paramSize @[10, 20, 30, 40]

proc writeToFile(f: File, a: varargs[string, `$`]) =
  for e in a: f.write e

# writeToFile stdout, 1, "two", true

type
  Person = object # value object
    name: string
    age: int

# var
#   p1 = Person(name: "Vlad", age: 37)
#   p2 = p1
# p2.name = "Lana"; p2.age -= 9
# echo p1, p2

type
  Employee = tuple # structural typing
    name: string
    id: int
  Employee2 = tuple[name: string, id: int]
  Employee3 = (string, int)

# var
#   e1: Employee = (name: "Vlad", id: 1)
#   e2: Employee3 = ("Vlad", 1)
#   (n, i) = e2 # tuple unpacking
#   a = [("a", 1), ("b", 2)]
#   b = {"c": 3, "d": 4}
# echo e1, e2, e1 == e2
# echo e1.name, e1[1]
# echo i, " ", n
# for (k, v) in a: echo k, " ", v
# for (k, v) in b: echo k, " ", v

type
  Node = ref object # reference object
    left, right: Node
    data: int

# var
#   n = Node(data: 1)
# echo n.data

type
  HelloType = proc (name: string): string # lambda type

proc hello(name: string): string = "Hello " & name
proc say(what: proc (name: string): string, name: string) = echo what name
proc say2(what: HelloType, name: string) = echo what name

# say(hello, "Vlad")
# say2(hello, "Lana")

type
  Person2 = ref object of RootObj
    name: string
    age: int
  Student = ref object of Person2 # object inheritance
    id: int

proc getStudentId(x: Person2): int = Student(x).id

# var
#   p = Person2(name: "Vlad", age: 37)
#   s = Student(name: "Vlad", age: 37, id: 1)
# echo p[], s[], p of Person2, p of Student, getStudentId s

type
  NodeKind = enum nkInt, nkFloat
  Node2 = ref object # object variant
    case kind: NodeKind
    of nkInt: intVal: int
    of nkFloat: floatVal: float

# var
#   i = Node2(kind: nkInt, intVal: 1)
#   f = Node2(kind: nkFloat, floatVal: 1.0)
# echo i[], f[]

import std/sequtils
from std/strutils import splitWhitespace

proc printMaxInt() =
  stdout.write "> "
  let maxInt = stdin.readLine.splitWhitespace.map(parseInt).max.`$`
  stdout.writeLine maxInt

# printMaxInt()

type
  Socket* = ref object of RootObj # export marker
    h: int # private field

proc `host=`*(s: var Socket, value: int) {.inline.} =
  echo "Socket.host: ", value
  s.h = value # property setter

proc host*(s: Socket): int {.inline.} =
  s.h # property getter

# var s = Socket()
# s.host = 34
# echo s.host

type
  Vector* = object
    x, y: float

proc `[]=`*(vec: var Vector, i: int, value: float) =
  case i
  of 0: vec.x = value
  of 1: vec.y = value
  else: assert false

proc `[]`*(vec: Vector, i: int): float =
  case i
  of 0: result = vec.x
  of 1: result = vec.y
  else: assert false

# var v = Vector(x: 10, y: 20)
# echo v
# v[0] = 3; v[1] = 4
# echo v[0], v[1]

type
  Expr = ref object of RootObj
  Lit = ref object of Expr
    v: int
  Plus = ref object of Expr
    a, b: Expr

proc newLit(v: int): Lit = Lit(v: v)
proc newPlus(a, b: Expr): Plus = Plus(a: a, b: b)

# dynamic dispatch
method eval(e: Expr): int {.base.} = quit "Abstract method"
method eval(l: Lit): int = l.v
method eval(p: Plus): int = eval(p.a) + eval(p.b)

# var
#   e = Plus(a: Lit(v: 1), b: Plus(a: Lit(v: 2), b: Lit(v: 3)))
#   e2 = newPlus(newLit 1, newPlus(newLit 2, newLit 3))
# echo eval e, eval e2

type
  AnError = object of CatchableError

# try:
#   raise newException(AnError, "Oh")
# except AnError as e:
#   echo "ERROR: ", e.msg
# except:
#   echo "Any error"
#   raise # re-raise exception
# finally:
#   echo "Finally"

type
  BinTree*[T] = ref object
    left, right: BinTree[T]
    data: T

proc newNode*[T](data: T): BinTree[T] =
  new result
  result.data = data

proc addNode*[T](root: var BinTree[T], node: BinTree[T]) =
  if root == nil: root = node
  else:
    var it = root
    while it != nil:
      if cmp(it.data, node.data) > 0:
        if it.left == nil: it.left = node; return
        it = it.left
      else:
        if it.right == nil: it.right = node; return
        it = it.right

proc addNode*[T](root: var BinTree[T], data: T) =
  addNode(root, newNode data)

iterator preorder*[T](root: BinTree[T]): T =
  var stack: seq[BinTree[T]] = @[root]
  while stack.len > 0:
    var node = pop stack
    while node != nil:
      yield node.data
      add stack, node.right
      node = node.left

# var tree: BinTree[int]
# for node in [10, 4, 1, 8, 7, 9, 5]: addNode tree, node
# for node in preorder tree: echo node

template `<>`(a, b: untyped): untyped = not (a == b)

# echo 5 <> 5, 5 <> 6

const debug = true

template log(msg: string) =
  if debug: stdout.writeLine msg

# log("ok")

template withFile(
  file: untyped, filename: string, mode: FileMode,
  body: untyped): untyped =
  var file: File
  if not open(file, filename, mode):
    raise newException(AnError, "cannot open file: " & filename)
  try: body
  finally: close file

# withFile(file, "template.txt", fmWrite):
#   writeLine file, "Vlad"
#   writeLine file, "Lana"

from std/math import sqrt

template liftProc(p: untyped): untyped =
  proc p[T](a: openarray[T]): auto =
    var t: T
    type outType = typeof p t
    result = newSeq[outType] len a
    for i in 0..<len a: result[i] = p a[i]

# liftProc sqrt
# echo sqrt [1.0, 2.0, 3.0, 4.0, 5.0]

macro echoInt(x: static[int]): untyped = echo x

# echo 1 + 2 + 3 # compile time evaluation

import std/macros

macro anAssert(arg: untyped): untyped =
  # echo treeRepr arg
  expectKind arg, nnkInfix
  expectLen arg, 3
  let
    op = newLit(" " & arg[0].repr & " ")
    lhs = arg[1]
    rhs = arg[2]
  result = quote do:
    if not `arg`:
      raise newException(AssertionDefect, $`lhs` & $`op` & $`rhs`)
  # echo repr result

# let
#   a = 1
#   b = 2
# anAssert a != b
# anAssert a == b

from std/strformat import fmt

type
  Animal = ref object of RootObj
    name: string
  Cat = ref object of Animal
  Mouse = ref object of Animal

proc newMouse(name: string): Mouse = Mouse(name: name)

# dynamic dispatch
method say(self: Animal) {.base.} = echo "Abstract animal"
method say(self: Cat) = echo self.name, " purrr"
method say(self: Mouse) = echo self.name, " pi-pi"
func `$`(a: Animal): string = fmt "Animal: {a.name}"

# var
#   cat: Animal = Cat(name: "Kotsyub")
#   mouse: Animal = newMouse("Sira")
#   animals: seq[Animal]
# animals.add(cat); animals.add(mouse)
# say cat
# say mouse
# for animal in animals: say animal
# echo cat

import std/wrapnils

type
  A = ref object
    a: A
    b: string
    c: ref int

# var
#   a1: A
#   a2 = A(b: "b")
# assert ?.a1.a.b == ""
# assert ?.a2.b == "b"
# a2.a = a2
# assert ?.a2.a.b == "b"
# assert ?.a2.a.c[] == 0

type
  Person3 = tuple[id: int, name: string]

# var
#   t1 = (1, "Vlad")
#   t2 = (id: 1, name: "Vlad")
#   t3: Person3 = (id: 1, name: "Vlad")
# echo t1, t2, t3

import std/sugar, std/tables, std/sets

let # list/table/set comprehensions
  lc1 = collect newSeq:
    for e in [-9, 1, 42, 0, -1, 9]: e
  lc2 = collect newSeq:
    for e1 in [1, 2]:
      collect newSeq:
        for e2 in [10, 20]:
          if e1 * 10 != e2: (e1, e2)
  dc1 = collect initTable(4):
    for k, v in [-9, 1, 42, 0, -1, 9]: {k: v}
  sc1 = collect initHashSet:
    for e in [-9, 1, 42, 0, -1, 9]:
      if e > 0: {e}

# echo lc1
# echo lc2
# echo dc1
# echo sc1

let
  add2 = func(a, b: int): int = a + b # lambda expression
  add3 = (a, b: int) => a + b # arrow function

# echo add2(1, 2)
# echo add3(3, 4)

let increment = func(a: int): int = a + 1 # anonymous function

# echo [1, 2, 3, 4, 5].map increment

let iter = iterator: int = # anonymous iterator
  for i in 0..4: yield i

# for i in iter: echo i

# (proc() = echo "Immediate anonymos")()

import std/json

# let
#   v = "value"
#   j = %*{ # JSON literal with interpolation and comments
#     "key": v,
#     # comment
#     "array": [1, 2, 3]
#   }
# echo j

import std/asyncdispatch

proc ahello(name: string) {.async.} =
  echo "Hello"
  await sleep_async 3000
  echo name

# wait_for ahello("Vlad")

type uint4 = distinct uint8

func `'u4`(lit: string): uint4 = (parseInt(lit) and 0x0f).uint4

func `$`(x: uint4): string = $x.uint8

# let i = 5'u4 # custom numeric literal
# echo i

func `\`(a, b: float): float = a / b # left associative (default)
func `^\`(a, b: float): float = a / b # right associative

# echo 12 \ 4 \ 8
# echo 12 ^\ 4 ^\ 8

# C FFI
proc cprintf(f: cstring) {.importc: "printf", varargs, header: "<stdio.h>".}

# cprintf "Works as %s", "expected"

type ArrIndex = enum first, second, third

# let a = [first: 1, second: 2, third: 3] # named array
# echo a, a[second]

# if (let a = 1; a == 1): echo 1 # statement list (st1; st2; expr)

# let a = block: # block expression
#   var fib = @[0, 1]
#   for i in 1..10: fib.add fib[^2] + fib[^1]
#   fib
# echo a

type
  Socket2* = ref object of RootObj
    host: int # private field

proc host*(s: Socket2): int = s.host
proc `host=`*(s: var Socket2, h: int) = s.host = h

# var s = Socket2(host: 1)
# echo s.host
# s.host = 2
# echo s.host

proc outer(a: int): int =
  proc inner(b: int): int = a + b # inner procedure
  inner 2

# echo outer 1

import std/algorithm

# var cities = ["Frankfurt", "Tokyo", "New York", "Kyiv"]
# cities.sort(func(a, b: string): int = cmp a.len, b.len)
# cities.sort((a, b) => cmp(a.len, b.len))
# echo cities

proc quotientRemainder2(a, b: int; q, r: var int) =
  q = a div b; r = a mod b

# var q, r: int
# quotientRemainder2 7, 4, q, r
# echo q, " ", r

proc quotientRemainder3(a, b: int; q, r: ptr int) =
  q[] = a div b; r[] = a mod b

# var q, r: int
# quotientRemainder3 7, 4, addr q, addr r
# echo q, " ", r

func quotientRemainer4(a, b: int): tuple[q, r: int] = (a div b, a mod b)

# var (q, r) = quotientRemainer4(7, 4)
# echo q, " ", r

iterator mints(a: var seq[int]): var int = # mutable iterator
  for i in 0..high a: yield a[i]

# var a = @[1, 2, 3, 4]
# for e in a.mints: e *= 10
# echo a

iterator triples(hi: int): (int, int, int) =
  var i = 0
  while i <= hi:
    yield (i, i * 10, i * 100)
    inc i

# for i, j, k in triples 5: echo i, " ", j, " ", k

converter toBool(s: string): bool = s == "yes"

# if "yes": echo "go"

# # try expression
# let a = try: parseInt("123x") except: -1 finally: echo "done"
# echo a

proc readFile(name: string) =
  let f = open(name, fmRead)
  defer: close f # try/finally alternative
  echo readLine f
  echo readLine f
  echo readLine f

# readFile("tutorial.nim")

type IO = object

proc pIO() {.tags: [IO]} = echo "ok"

# proc noIO() {.tags: []} = pIO() # error

template declareVar(x: untyped, T: typedesc, v: T) =
  var x: T = v

# declareVar a, int, 1
# echo a

template maxval(T: typedesc[int]): T = high int
template maxval(T: typedesc[float]): T = Inf

# echo maxval int, " ", maxval float

# assert typeof(1 + 2) is int
