import std/[algorithm, sets, sugar]

#[
- Big O notation = time complexity of the worst-case scenario, unless otherwise
  stated
    - How many algorithm steps => for input of length N (concrete complexity)
    - How algorithm steps change => on input increase (complexity grows)
    - How algorithm steps change => on large inputs (asymptotic complexity)
    - O(1) = constant time (does not depends on the input lenght)
    - O(lon(n)) = logarithmic time (input * => step +)
    - O(n) = linear time (proportional growth)
    - o(n^2) = quadratic time (exponetial growth)
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

# O(n^2)
proc bubble_sort2[T](a: var openArray[T], cmp: proc(a, b: T): int = cmp) =
  for i in countdown(a.high, 1):
    for j in 1..i:
      if cmp(a[j - 1], a[j]) > 0:
        swap(a[j - 1], a[j])

proc bubble_sort[T](a: var openArray[T], cmp: proc(a, b: T): int = cmp) =
  var n = a.high
  while n >= 1:
    var m = 0
    for j in 1..n:
      if cmp(a[j - 1], a[j]) > 0:
        swap(a[j - 1], a[j])
        m = j
    n = m

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

# O(n)
func hasDuplicates[T](a: openArray[T]): bool =
  var s: HashSet[T]
  for e in a:
    if s.containsOrIncl(e): return true

echo [1, 2, 3].hasDuplicates, " ", [1, 2, 3, 1].hasDuplicates
