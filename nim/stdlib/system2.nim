# integer
echo 5 div 2 # integer devision
echo -8 mod 3 # signed modulo
 # unsigned modulo arithmetic
echo -8 +% 3, " ", -8 -% 3, " ", -8 *% 3, " ", -8 /% 3, " ", -8 %% 3
var
  a = 1
  b = 2
swap(a, b)
echo a, " ", b

# string
echo "vlad".len # string length
echo "Vlad " & "Lana" # string concatenation
var s = "Vlad"
s.add('!') # in-place appends char
echo s
s &= "?" # in-place concatenation
echo s

# array[I, T], range[T], params: openArray[T], varargs[T]
# seq[T]
var
  s2 = newSeq[int](5) # initialized with 0s
  s3 = newSeqOfCap[int](5) # empty sequence
echo s2, " ", s3
s2.setLen(10) # expands / shrinks sequence initialized with 0s
echo s2
echo s2.len # sequence length
s2 = @[1, 2, 3] # converts an array to sequence
echo s2
s2.add(4) # in-place appends element
echo s2
echo s2.pop # pops last element
echo s2
echo s2 & 4, " ", 0 & s2, " ", s2 & @[4] # appends / prepends element / sequence
echo s2[^1] # last element
echo s2[0..1] # inclusive slice
echo s2[0..^1]
echo s2[0..<1] # exclusive slice
echo 2 in s2, " ", s2.contains(2), " ", s2.find(2) >= 0 # in / contains / find
echo s2.max, " ", s2.min

# set[T]
var
  s4 = {1, 2, 3, 4}
  s5 = {3, 4, 5, 6}
echo 1 in s4, " ", 10 notin s4 # element membership
echo {1, 3} <= s4 # (strict) subset
echo s4.card # set cardinality
echo s4 + s5 # union
echo s4 * s5 # intersaction
echo s4 - s5 # difference

# ordinals (bool, char, integer, enum)
echo 'a'.ord # converts value to position
echo 98.chr # converts position to value
var c = 'a'
c.inc; c.dec # in-place increments / decrements
echo c
echo 'a'.succ, " ", 'b'.pred # successor / predecessor value
echo int8.high, " ", int8.low # max / min value of type

# types and values
echo static(1 + 2) # compile-time evaluation
echo type(1 + 2) # type of expression
echo @[1, 2] is seq[int], " ", 1 isnot char # checks value is of type
echo newException(ValueError, "oh") of CatchableError # check value is instance
echo typeof(@[1, 2]) # get type of expression

# exceptions
type AnError = object of CatchableError # root of catchable exceptions
try:
  raise newException(AnError, "oh")
except AnError as e:
  echo e.name, " ", e.msg, " ", e.parent.type

# defer = finally
proc p1() =
  echo "init"
  defer: echo "finalize"
  # raise newException(ValueError, "oh")
  echo "process"

p1()
