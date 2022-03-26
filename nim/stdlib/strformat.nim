import std/strformat

# Standard format specifiers
# [[fill]align][sign][#][[0]minWidth][.floatPrecis|strMaxWidth][type]
# - align
#     - < left aligned (default for strings)
#     - ^ centered
#     - > right aligned (default for numbers)
# - sign
#     - + both positive and negative numbers
#     - - only for negative numbers (default)
#     - <space> leading space on positive numbers
# - # indicates 0b, 0o, 0x prefix
# - type
#     - b binary
#     - o octal
#     - d decimal (default)
#     - x hexadecimal
#     - f fixed point
#     - e scientific
#     - g fixed point or scientific (for small or large numbers)

let name = "Vlad"

echo fmt"{name}\n" # raw string
echo fmt "{name}\n" # respects escape sequences
echo fmt "{{name}}" # escape interpolation

# string formatting
echo fmt "{name:_<8}" # left aligned, min width, _ padding
echo fmt "{name:_>8}" # right aligned, min width, _ padding
echo fmt "{name:_^8}" # centered, min width, _ padding
echo fmt "{name:.2}" # left aligned (default), max width

# integer formatting
let iNum = 1234
echo fmt "{iNum:8}" # right aligned (default), space padding
echo fmt "{iNum:08}" # right aligned (default), zero padding
echo fmt "{iNum:+}" # explicit sign
echo fmt "{iNum:#b}" # binary representation, 0b prefix

# float formatting
let fNum = 1.234
echo fmt "{fNum:8f}" # right zero padding
echo fmt "{fNum:8.2f}" # right aligned (default), precision, right space padding
echo fmt "{fNum:08.2f}" # right aligned (default), precision, left zero padding
echo fmt "{fNum:.3e}" # scientific format, precision
echo fmt "{(if fNum == 0.0: fNum else: 1 / fNum):.4}" # expression
