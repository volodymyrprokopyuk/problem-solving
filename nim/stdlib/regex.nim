import pkg/regex

# ** Regex interface

# Regex compilation

const ctRegex = re"\w\d" # complie-time regex compilation

let rtRegex = re"\w\d" # run-time regex compilation

# Simple match + bool

if "A cat".match re". cat": echo "match" # whole string match => bool
if re"cat" in "A cat": echo "match" # substring match => bool

if s.startsWith(re"[A-Z]"): echo "match"
if s.endsWith(re"t{2}y"): echo "match"

# Regex split

for w in s.split(re"\s"): echo w # split on regex
for w in s.splitIncl(re"(\s)"): echo w # split on regex + include separator

let s = "A cat catty"
var m: RegexMatch

# Whole string match + Match

if s.match(re".(?: (\w+))+", m): # whole string match => Match
  # multiple matches as slices of a group 0
  for slice in m.group(0): echo s[slice]
  # multiple matches as strings of a group 0
  for capture in m.group(0, s): echo capture
  echo m.groupFirstCapture(0, s)
  echo m.groupLastCapture(0, s)

if s.match(re".(?: (?P<pet>\w+))+", m): # whole string match => Match
  # multiple matches as slices of a named group
  for slice in m.group("pet"): echo s[slice]
  # multiple matches as strings of a named group
  for capture in m.group("pet", s): echo capture
  echo m.groupFirstCapture("pet", s)
  echo m.groupLastCapture("pet", s)

# Substring match + Match

if s.find(re"(\w{3,})", m): # substring first match => bool + Match
  echo m.groupFirstCapture(0, s)

for m in s.findAll(re"(?P<pet>\w{3,})"): # substring all matches => [Match]
  echo m.groupFirstCapture("pet", s)

# Regex replace

echo s.replace(re"c(?=a)", "C") # replace all accurences
echo s.replace(re"(\w{3,}) (\w{3,})", "$2 $1") # replace with captured content
echo s.replace(re"(?P<a>\w{3,}) (?P<b>\w{3,})", # replace with proc(Match)
               proc(m: RegexMatch, s: string): string =
                 m.groupFirstCapture("b", s) & " " & m.groupFirstCapture("a", s))
