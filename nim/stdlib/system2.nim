# string
echo "vlad".len # string length
echo "Vlad " & "Lana" # string concatenation
var s = "Vlad"
s.add('!') # in-place appends char
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
s2 = @[1, 2, 3] # converts array to sequence
echo s2
s2.add(4) # in-place appends element
echo s2
echo s2.pop # pops last element
echo s2
echo s2 & 4, " ", s2 & @[4] # appends element / sequence
echo s2[^1] # last element
echo s2[0..1] # inclusive slice
echo s2[0..^1]
echo s2[0..<1] # exclusive slice

# set[T]
var
  s4 = {1, 2, 3, 4}
  s5 = {3, 4, 5, 6}
echo 1 in s4 # element membership
echo {1, 3} <= s4 # (strct) subset
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

# exceptions
type AnError = object of CatchableError
try:
  raise newException(AnError, "oh")
except AnError as e:
  echo e.name, " ", e.msg, " ", e.parent.type
