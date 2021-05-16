# J

- J array programming with implicit, structural, strong, dynamic typing
- Single data type multi dimensional array + sparse array (value + index)
- J concise notation of computation = power for rapid algorithm development
- Tacit programming = point-free style + function composition
- Evaluate right-to-left (no precedence, right-associative), read left-to-right
- Literals: `_1` negative, `_` infinity, `1j2` complex
- Common operations: `=.` assignment, `NB.` comment
- Arithmetic operations: `+` conjugate / addition, `-` negate / subtraction, `*` signum
  / multiplication, `%` reciprocal / division, `%:` square root `sqrt`, `^` power
  `expt`, `^.` natural logarithm `log`, `10 ^.` decimal logarithm `log <> 10`, `<.`
  floor / minimum, `>.` ceiling / maximum
- Array operations: `#` length / copy, `{` index, `$` shape of / reshape, `,` append
- Noun = data, verb = function, modifier = verb modification
- Monadic verb `v n`
- Diadic verb `n0 v n1`
- Fork `(v0 v1 v2) n = (v0 n) v1 (v2 n)`
