{.hint[XDeclaredButNotUsed]: off.}
{.hint[DuplicateModuleImport]: off.}
{.warning[UnusedImport]: off.}

type User = object

proc say(user: User) = echo "Hi proc"

# method say(user: User) {.base.} = echo "Hi method"

# let user = User()
# say user

from std/sugar import `=>`, dup
from std/sequtils import toSeq, map, mapIt
from std/strutils import join, toUpperAscii, toBin
from std/strformat import fmt

# let nums = [1, 2, 3, 4]
# echo nums.map(x => $x).join ", "
# echo nums.mapIt($it).join ", "
# echo nums.mapIt(fmt "{it}").join ", "

func sumFirstN(n: int): int =
  if n >= 0:
    for i in 1..n: result += i
  else:
    for i in countdown(-1, n): result += i

# echo sumFirstN 0, " ", sumFirstN 1, " ", sumFirstN 10, " ",
#   sumFirstN -1, " ", sumFirstN -10

from std/math import isNaN, `^`, pow, sqrt

# let
#   a = Inf
#   b = NegInf
#   c = NaN
# echo a == Inf, b == NegInf, c == NaN, isNaN c
# echo 2^3, " ", pow(2, 3.1), " ", sqrt 2.0

type # not compatible with the base type, operations must be defined
  Time = distinct float
  Dist = distinct float

# let
#   t = Time(1.0)
#   d = Dist(2.0)
# echo t.float + d.float

type # compatible witht the base type, operations are reused
  Day = range[1..31]
  Prob = range[0.0..1.0]

# let
#   d: Day = 30
#   p: Prob = 0.5
# echo d, " ", p

type
  Direction = enum north, east, south, west
  Weather = array[Direction, string]

# for d in Direction: echo ord d, " ", d
# let w: Weather = # named array
#   [north: "cold", east: "sunny", south: "hot", west: "rainy"]
# echo w[east], " ", w[Direction.south]

proc printAscii() =
  for i in 32..127:
    if i mod 16 == 0: stdout.write fmt "\n{i:>3} "
    stdout.write chr i, " "

# printAscii()
# echo 'A', '\65', '\x41', '\n', '\\', '\''

# var # char set
#   alpha = {'a'..'z', 'A'..'Z'}
#   digit: set[char] = {'0'..'9'}
#   alphanum = alpha + digit
# echo 'a' in alphanum, " ", '_' notin alphanum, " ", alpha.contains 'V', " ",
#   alpha * digit
# alpha.incl '_'; alpha.excl '?'
# echo '_' in alpha, alpha - {'a'..'z'}

type # int set
  ChessRange = range[0..64]
  ChessPos = set[ChessRange]

# let c: ChessPos = {0.ChessRange..3.ChessRange}
# echo c

type # enum set
  LangFeat = enum lfCompiled, lfInterpreted, lfSelfHosted
  LangDesc = set[LangFeat]

# let nim = {lfCompiled, lfSelfHosted}
# echo nim

# let s = "Vlad" # subscript [i], slice[i..j]
# echo len s, " ", high s, " ", s[0], " ", s[1..3], " ",
#   "Hello " & s, " ", s[^3..^1]
# let
#   r = r"raw\n"
#   m = """
# multi\n
# line\t
#   """
# echo r
# echo m
# echo "\u03b1 \u03b2"
# var v = "Vlad"
# v.add '!'
# echo v

proc localType() =
  type LocalInt = range[0..10]
  let
    i = LocalInt 1
    j = 2.LocalInt
  echo i, " ", j

# localType()

proc globalCounter() =
  # global variable with local visibility
  var counter {.global.} = 0
  counter += 1
  echo counter

# for _ in 1..5: globalCounter()

# if (let x = 10; x > 0): echo "positive" else: echo "non positive"

# when sizeof(int) == 4: # no new scope
#   var arch = "32bit"
# elif sizeof(int) == 8:
#   var arch = "64bit"
# else: discard
# echo arch # visible outside of when

# var s = "Vlad"
# for c in s.items: echo c
# for c in s.mitems: c = c.toUpperAscii
# echo s

import std/with

type
  Laptop = object # value object
    brand: string
    model: string
    year: int

# var
#   laptop: Laptop
# with laptop:
#   brand = "Lenovo"
#   model = "ThinkBook"
#   year = 2021
# echo laptop

# var
#   l1 = Laptop(brand: "A")
#   l2: Laptop
# l2 = l1 # copy on assignment
# l1.brand = "B"
# echo l1, " ", l2

# var
#   a: array[4, int] # fixed
#   s: seq[int] = newSeq[int](4) # elastic + capacity
# echo a, " ", s

# var
#   a = 1
#   p: ptr int
# p = addr a
# p[] += 1
# echo p[], " ", cast[int](p) # unsafe, destroys type safety

# var # ptr => unmanaged, typed pointer (pointer => unmanaged, untyped pointer)
#   p, p2: ptr int
# p = create int # manual allocation (create, alloc)
# p[] = 1
# p2 = p
# p2[] += 1
# echo p[], " ", p2[]
# dealloc p # manual deallocation

# var # managed only typed references
#   r, r2: ref int
# new r # manual allocation + automatic GC
# r[] = 1
# r2 = r
# r2[] += 1
# echo r[], " ", r2[]

type
  Friend = ref object
    name: string
    next: Friend

func newFriend(name: string): Friend = Friend(name: name)

func append(names: openArray[string]): Friend =
  var lastFriend: Friend
  for name in names:
    if lastFriend != nil:
      let nextFriend = newFriend name
      lastFriend.next = nextFriend
      lastFriend = nextFriend
    else:
      result = newFriend name
      lastFriend = result

func prepend(names: openArray[string]): Friend =
  var lastFriend: Friend
  for name in names:
    if lastFriend != nil:
      result = newFriend name
      result.next = lastFriend # assignment copies the reference, not the object
      lastFriend = result
    else:
      result = newFriend name
      lastFriend = result

func remove(root: Friend, name: string): Friend =
  result = root
  var lastFriend, nextFriend: Friend
  nextFriend = root
  while nextFriend != nil: # equality compares references, not the content
    if nextFriend.name == name:
      if nextFriend != result:
        lastFriend.next = nextFriend.next
      else:
        result = nextFriend.next
      break
    lastFriend = nextFriend
    nextFriend = nextFriend.next

proc print(root: Friend) =
  var nextFriend = root
  while nextFriend != nil:
    echo nextFriend.name
    nextFriend = nextFriend.next

# let
#   names = ["Vlad", "Lana", "Orysia", "Kotsyub"]
#   straight = append names
#   reversed = prepend names
# print straight
# print reversed
# var
#   friends = append names
# friends = remove(friends, "Vlad")
# print friends

# let
#   a = newFriend("Vlad")
#   b = newFriend("Vlad")
# echo a == b, " ", a[] == b[] # references vs content comparison

from std/algorithm import sort, SortOrder

# var
#   a = @[1, 5, 3, 7, 2, 4, 8, 9, 6]
#   b = a.dup(insert 0, sort) # in-place modification on a copy
#   c = a.dup:
#     insert 0
#     sort SortOrder.Descending
# echo a, " ", b, " ", c

proc varResult(a: var int): var int = a

# var a = 1
# varResult(a) += 1
# echo a

type
  ValObj = object
    num: int

proc modifyValObj(o: var ValObj) = o.num += 1

# var o = ValObj()
# modifyValObj o
# echo o

type
  RefObj = ref object
    num: int

func `$`(o: RefObj): string = fmt "(num: {o.num})"

proc modifyRefObj(o: RefObj) = o.num += 1

# var o = RefObj()
# modifyRefObj o
# echo o

func sumOpenArray(a: openArray[int]): int =
  for e in a: result += e

func sumVarargs(a: varargs[int]): int =
  for e in a: result += e

# echo sumOpenArray [1, 2, 3, 4, 5]
# echo sumVarargs(1, 2, 3, 4, 5)

func square[T](x: T): T = x * x # same algorithm different types

# echo square 2, " ", square 2.0, " ", square {1, 2}

func cube(x: int or float): auto = x * x * x

# echo cube 2, " ", cube 2.0

proc identity[T](x: T): T = x

# var p: proc(x: int): int # procedure variable
# p = identity
# echo p 1

type IntProc = proc(x: int): int

proc applyIntProc(p: IntProc, x: int): int = p x

# var addOne: IntProc = proc(x: int): int = x + 1
# echo applyIntProc(addOne, 1), " ", applyIntProc(x => x * 10, 1)

func scanDigits(input: string): string =
  var pos = 0
  func nextDigit(): char = # internal procedure, closure
    while pos < input.len:
      let c = input[pos]
      inc pos
      if c in {'0'..'9'}: return c
  result = newString(input.len)
  while (let d = nextDigit(); d != '\0'): result.add d

# echo scanDigits "0a1bc23d45", " ", scanDigits "abc", " ", scanDigits ""

iterator scanDigits2(input: string): char =
  var pos = 0
  while pos < input.len:
    let c = input[pos]
    inc pos
    if c in {'0'..'9'}: yield c

# for d in scanDigits2 "0a1bc23d45": stdout.write d

func addN[T](n: T): auto =
  (proc(x: T): T = x + n)

# let add2 = addN 2
# echo add2 1

# let a = (0..5).toSeq
# echo a.map(proc(x: int): int = x * x)
# echo a.map(x => x * x)
# echo a.mapIt(it * it)

converter int2bool(x: int): bool {.inline.} = x != 0

# let a = 1
# if a: echo "ok"

func swapTuple[T, U](x: tuple[a: T, b: U]): tuple[a: U, b: T] =
  (x.b, x.a)

# echo swapTuple (1, "a")

func swapTuple2[T, U](x: (T, U)): (U, T) =
  let (a, b) = x
  result = (b, a)

# echo swapTuple2 (1, "a")

type # less performant
  Shape = ref object of RootObj # reference object => heap
  Rect = ref object of Shape # distinct types
    width, height: float
  Circle = ref object of Shape
    radius: float

method draw(shape: Shape) {.base.} = echo "abstract method"

method draw(rect: Rect) = echo "Rect" # dynamic dispatch

method draw(circle: Circle) = echo "Circle"

# let shapes = [Rect(), Circle()]
# for shape in shapes: draw shape

type # more performant
  ShapeKind = enum skRect, skCircle
  ShapeVariant = object # value object + enum discriminator => stack
    case shapeKind: ShapeKind
    of skRect:
      width, height: float
    of skCircle:
      radius: float

proc draw(shape: ShapeVariant) = # static dispatch
  case shape.shapeKind:
  of skRect: echo "Rect"
  of skCircle: echo "Circle"

# let shapes2 =
#   [ShapeVariant(shapeKind: skRect), ShapeVariant(shapeKind: skCircle)]
# for shape in shapes2: draw shape

# let a = 12345 # cast breaks type safety
# echo cast[float](a) # reitnerpret the same binary representation
# echo a.float # type conversion creates new binary structure

type
  Rec = object
  RecRef = ref object of RootObj
  SubRecRef = ref object of RecRef

proc `=destroy`(r: var Rec) =
  echo "Rec =destroy"

proc `=destroy`(r: var typeof RecRef()[]) = # ref object
  echo "RecRef =destory" # --gc:arc

proc `=destroy`(r: var typeof SubRecRef()[]) = # sub object
  echo "SubRecRef =destroy" # --gc:arc

# let
#   r = Rec()
#   rr = RecRef()
#   srr = SubRecRef()

from std/math import Pi

# echo Pi, " ", math.sin Pi

import std/macros

#[
- Tempalte is a tamplate-based macro
- `{.gensym.}` (default) template private, hygienic variables (e. g.
  type, var, let, const)
- `{.inject.}` template variables are exposed to the instantiation
  scope (e. g. proc, iterator, converter)
]#

#[
- Macro is a compile-time funciton that transforms AST
- `untyped` is the only possible return value `NimNode` for a macro
  (can be omitted)
- all macro params (exept `static[T]` params which are of type `T`)
  and the predefined `result` are `NimNode` inside a macro
- `static[T]` passes compile-time constant expression to a macro as an
  ordinary value (not a `NimNode`)
- `typed` applies syntax checking + semantic checking, `NimNode` + type
  infomration
- `untyped` applies only syntax checking (no semantic checking), `NimNode`
  without type information
- `expectKind`, `expectLen` explicity semantic checking inside a macro
- result of macro expansion is always chacked by the compiler
- `parseStmt(s: string): NimNode` string-based code generation
- `quote do:` ` ``NimNode symbol`` ` declarative code generation
- `newLit`, `newIdentNode`, `newTree` programmatic code generation
- last argument to a `proc`, `template`, `macro` can be a code block
- macros and templates can be used as `{.pagma.}` attached to procedures,
  type names and type expressions
]#

template runTimeEcho(s: string): untyped = echo s

macro compileTimeEcho(s: string): untyped = echo s

# runTimeEcho "ok"
# compileTimeEcho "ok"

macro fromString(code: static[string]): untyped = parseStmt code

# fromString """echo "ok" """

macro printFields(obj: untyped, fields: varargs[untyped]): untyped =
  var code = ""
  for field in fields: code.add(fmt "echo {obj}.{field}\n")
  echo code
  parseStmt code

type Point3D = object
  x, y, z: float

# let p = Point3D(x: 1.0, y: 2.0, z: 3.0)
# printFields p, x, y, z

# echo "a": # last argument to proc, template, macro as a code block
#   "b"

macro m(x: untyped): untyped =
  echo "ok"
  echo x

# var x = 1.0
# m y

macro debugExpr(e: untyped): untyped =
  let strLit = e.toStrLit
  quote do:
    echo `strLit`, " => ", `e`

# let
#   a = 2.0
#   b = 3.0
# debugExpr sqrt(a) + b

# dumpTree: # dump AST structure
#   echo "a + b", " => ", a + b
# dumpAstGen: # dump AST generation
#   echo "a + b", " => ", a + b

macro debugExpr2(expr: varargs[untyped]): untyped =
  result = nnkStmtList.newTree
  for e in expr:
    let command = nnkCommand.newTree
    command.add newIdentNode "echo"
    command.add newLit repr e
    command.add newLit " => "
    command.add e
    result.add command

# let
#   a = 2.0
#   b = 3.0
# debugExpr2 sqrt(a) + b, a^2 - b

macro anAssert(arg: untyped): untyped =
  # echo treeRepr arg
  arg.expectKind nnkInfix
  arg.expectLen 3
  let
    op =  newLit repr arg[0]
    lhs = arg[1]
    rhs = arg[2]
  quote do:
    if not `arg`:
      raise newException(AssertionDefect, $`lhs` & `op` & $`rhs`)

# let
#   a = 1
#   b = 2
# anAssert a != b


# dumpAstGen:
#   proc p(i: int) =
#     let pn = "p"
#     echo p
#     echo "ok"

macro procName(p: untyped): untyped =
  p.expectKind nnkProcDef
  let
    pn = name p
    pb = body p
    echoNode = nnkCommand.newTree(newIdentNode "echo", newLit $pn)
  pb.insert 0, echoNode
  result = p

proc printProcName(s: string) {.procName.} = echo s # use macro as pragma

# printProcName "ok"

type
  Seqs = object
    a, b, c: seq[int]

# dumpAstGen: # print macro target structure
#   for e in ss.a: stdout.write e, " "

macro iterateSeqs(fields: openArray[string], iter: untyped): untyped =
  # echo treeRepr iter # print parameters structure
  iter.expectKind nnkIteratorDef
  let
    iterBody = body iter
    objName = iter.params[1][0]
  for field in fields:
    let forNode =
      nnkStmtList.newTree(
        nnkForStmt.newTree(
          newIdentNode "e",
          nnkDotExpr.newTree(newIdentNode $objName, newIdentNode $field),
          nnkStmtList.newTree(
            nnkCommand.newTree(
              nnkDotExpr.newTree(newIdentNode "stdout", newIdentNode "write"),
              newIdentNode "e", newLit " "))))
    iterBody.insert(iterBody.len, forNode)
  result = iter

iterator items(ss: Seqs): int {.iterateSeqs(["a", "b", "c"]).} = discard

# let ss = Seqs(a: @[1], b: @[2, 3], c: @[4, 5, 6])
# for e in ss: stdout.write e, " "
