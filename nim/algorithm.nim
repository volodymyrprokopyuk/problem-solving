import std/[strformat, sequtils, algorithm, sets, sugar]
{.hint[XDeclaredButNotUsed]: off.}
{.warning[UnusedImport]: off.}

#[
- Big O notation = time complexity of the worst-case scenario, unless otherwise
  stated
- How many algorithm steps => for input of length N (concrete complexity) How
- algorithm steps change => on input increase (complexity growth) How algorithm
- steps change => on large inputs (asymptotic complexity) Big O provides general
- categories of algorithms and ignores constants O(n) = O(2n) = O(10n + 100) Bit
- O only takes into account the highest order of n O(n^3) = O(n^3) + O(n^2) +
- O(n) O(1) = constant time (does not depends on the input lenght) O(lon(n)) =
- logarithmic time (input * => step +) O(n) = linear time (proportional growth)
- o(n^2) = quadratic time o(2^n) = exponetial time (input + => step *)
]#

template valueError(message: string) =
  raise newException(ValueError, message)

# O(n)
func linear_search*[T](a: openArray[T], x: T): int =
  result = -1
  for i, e in a:
    if x == e: return i

# O(log(n)) sorted array
func binary_search*[T](a: openArray[T], x: T): int =
  result = -1
  var (l, h) = (a.low, a.high)
  while l <= h:
    let m = (l + h) div 2
    if x == a[m]: return m
    elif x < a[m]: h = m - 1
    else: l = m + 1

# let
#   e: array[0, int] = []
#   a = [5, 3, 6, 1, 8, 2, 7, 9, 0, 4]
#   b = a.sorted
# echo a.linear_search(7), " ", a.linear_search(10), " ", e.linear_search(0)
# echo b.binary_search(7), " ", b.binary_search(10), " ", e.binary_search(0)

# O(1) sorted array
func median[T](a: openArray[T]): float =
  if a.len == 0: valueError("median: empty array")
  let m = a.len div 2
  if a.len mod 2 == 0: (a[m - 1] + a[m]) / 2
  else: a[m].float

# echo [1].median, " ", [1, 2].median
# echo [1, 2, 3].median, " ", [1, 2, 3, 4].median

# O(n^2) comparison, O(n^2) swap
proc bubble_sort2[T](a: var openArray[T], cmp: proc(a, b: T): int = cmp) =
  for i in countdown(a.high, 1):
    for j in 1..i:
      if cmp(a[j - 1], a[j]) > 0: # cmp & swap in nested loop
        swap(a[j - 1], a[j])

# O(n^2) comparison, O(n^2) swap
proc bubble_sort[T](a: var openArray[T], cmp: proc(a, b: T): int = cmp) =
  var n = a.high
  while n >= 1:
    var m = 0
    for j in 1..n:
      if cmp(a[j - 1], a[j]) > 0:
        swap(a[j - 1], a[j])
        m = j
    n = m

# O(n^2) comparison, O(n) swap
proc selection_sort[T](a: var openArray[T], cmp: proc(a, b: T): int = cmp) =
  for i in 0..<a.high:
    var k = i
    for j in i + 1..a.high:
      if cmp(a[k], a[j]) > 0: k = j # cmp in nested loop
    if k != i: swap(a[i], a[k]) # only swap in flat loop

# O(n^2) comparison, O(n^2) shift
proc insertion_sort[T](a: var openArray[T], cmp: proc(a, b: T): int = cmp) =
  for i in 1..a.high:
    let e = a[i]
    var j = i - 1
    while j >= 0 and cmp(a[j], e) > 0: # eatly exit from the nested loop
      a[j + 1] = a[j] # shift in nested loop
      j.dec
    a[j + 1] = e

# var
#   e: array[0, int] = []
#   a = [1]
#   b = [2, 1]
#   c = [5, 3, 6, 1, 8, 2, 7, 9, 0, 4]

# e.bubble_sort
# a.bubble_sort
# b.bubble_sort
# c.bubble_sort((a, b) => -cmp(a, b))
# echo e, " ", a, " ", b, " ", c

# e.selection_sort
# a.selection_sort
# b.selection_sort
# c.selection_sort((a, b) => -cmp(a, b))
# echo e, " ", a, " ", b, " ", c

# e.insertion_sort
# a.insertion_sort
# b.insertion_sort
# c.insertion_sort((a, b) => -cmp(a, b))
# echo e, " ", a, " ", b, " ", c

# O(n)
func hasDuplicates[T](a: openArray[T]): bool =
  var s: HashSet[T]
  for e in a:
    if s.containsOrIncl(e): return true

# echo [1.0, 2.0, 3.0].hasDuplicates, " ", [1, 2, 3, 1].hasDuplicates

# O(n)
func isPalindrome(s: string): bool =
  result = true
  var (i, j) = (0, s.high)
  while i <= j:
    if s[i] != s[j]: return false
    i.inc; j.dec

# for s in ["", "a", "racecar", "kayak", "deified", "abcdba"]:
#   echo s.isPalindrome

func passwords2(n: int, r = 'a'..'c', all = true): seq[string] =
  var a = @[""]
  for _ in 0..<n:
    var b = newSeq[string]()
    for s in a:
      for c in r: b.add(s & c)
    a = b
    if all: result &= b else: result = b

# O(nm)
func `*`(a: seq[string], r: Slice[char]): seq[string] =
  for s in a:
    for c in r: result.add(s & c)

# O(26^n)
func passwords(n: int, r = 'a'..'c', all = true): seq[string] =
  var a = @[""]
  for _ in 0..<n:
    a = a * r
    if all: result &= a else: result = a

# echo 0.passwords
# echo 1.passwords
# echo 2.passwords
# echo 2.passwords(all = false)

# O(n)
func mergeSorted[T](a, b: openArray[T]): seq[T] =
  var (i, j) = (0, 0)
  while i <= a.high or j <= b.high:
    if i > a.high:
      result &= b[j..b.high]
      return
    elif j > b.high:
      result &= a[i..a.high]
      return
    elif a[i] < b[j]:
      result &= a[i]
      i.inc
    else:
      result &= b[j]
      j.inc

# let e: array[0, int] = []
# echo e.mergeSorted(e)
# echo [1].mergeSorted([2])
# echo [1, 3, 7, 8, 9].mergeSorted([2, 4, 5, 6])

# O(nm)
func find(n, h: string): int =
  result = -1
  for i in 0..h.high - n.high:
    var f = true
    for j in 0..n.high:
      if n[j] != h[i + j]:
        f = false
        break
    if f: return i

# echo "".find("")
# echo "a".find("a")
# echo "a".find("b")
# echo "a".find("bab")
# echo "ab".find("bcab")
# echo "ab".find("bcabc")
# echo "abx".find("bcabc")

#[
- HashTable O(1) insert, O(1) lookup
- Uses: mapping logic, object field-value, sets (index with no values)
- key1.hash == key2.hash => collision => associative array with O(1) linear
  search: same key.hash [[key1, value1], [key2, value2]]
- Bigger number of keys => more collissions with fixed storage
- Bigger storage + better hash function => less collisions
- Ideal load factor = 0.7 (7 keys in 10 cells) => expand storage + hash function
- Set = ADT on top of HashTable
]#

#[
- Stack = ADT, LIFO restricted array (ordered temporary data)
    - Data can be inserted only at the end / top of stack (push)
    - Data can be deleted only from the end / top of stack (pop)
    - Only the last element / top of a stack can be read (peek)
- Queue = ADT, FIFO restricted array (ordered temporary data)
    - Data can be inserted onlty at the end / back of queue (enqueue)
    - Data can be delete only from the beginning / front of queue (dequeue)
    - Onnly the element from the front can be read (peek)
]#

type Stack[T] = seq[T]

proc push[T](s: var Stack[T], v: T) = s.add(v)

func top[T](s: Stack[T]): T = s[s.high]

# var s: Stack[int]
# s.push(1)
# s.push(2)
# echo s, " ", s.len
# echo s.pop
# echo s.top

func lintParens(
  code: string,
  parens: varargs[(char, char)] = {'(': ')', '[': ']', '{': '}', '<': '>'}):
  tuple[success: bool, position: int, error: string] =
  let parens = @parens
  func isOpen(c: char): bool =
    for (a, _) in parens:
      if c == a: return true
  func isClose(c: char): bool =
    for (_, b) in parens:
      if c == b: return true
  func close(c: char): char =
    for (a, b) in parens:
      if c == a: return b
  result = (true, -1, "")
  var stack: Stack[char]
  for i, c in code:
    if c.isOpen: stack.push(c)
    elif c.isClose:
      if stack.len == 0: return (false, i, fmt "extra close {c}")
      let b = stack.pop.close
      if c != b: return (false, i, fmt "mismatch: expected {b}, got {c}")
  if stack.len != 0: return (false, code.high, fmt "extra open {stack.top}")

# for code in ["a(b[c{d<e>}])", "(a)b]", "<a(b]>", "{a(b[c<d>])"]:
#   echo code, " ", code.lintParens

type Queue[T] = seq[T]

proc enqueue[T](q: var Queue[T], v: T) = q.add(v)

proc dequeue[T](q: var Queue[T]): T =
  result = q.front
  q.delete(0..0)

func front[T](q: Queue[T]): T = q[q.low]

# var q: Queue[int]
# q.enqueue(1)
# q.enqueue(2)
# echo q, " ", q.len
# echo q.dequeue
# echo q.front