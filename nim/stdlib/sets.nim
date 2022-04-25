import std/sets

var
  a = toHashSet([1, 2, 3])
  b = toHashSet([3, 4, 5])
  c: HashSet[int] # initialized by default

echo a[1] # index by value
echo a.len, " ", a.card # set cardinality
echo a < b # [strict] subset
echo a == b # set equality
# union, intersection, difference, symmetricDifference
echo a + b, " ", a * b, " ", a - b, " ", a -+- b
echo 3 in a, " ", 4 notin a # membership via contains
echo a.containsOrIncl(3)
echo a.missingOrExcl(4)
a.incl(toHashSet([10, 20])) # include element or set
echo a
a.excl(toHashSet([10, 20])) # exclude element or set
echo a
echo a.map(proc(e: int): string = $e) # set transformation
for e in b: echo e # iterate over set elements
echo b.pop # excludes arbitrary element
echo c
