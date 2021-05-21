# J

- J array programming with implicit, structural, strong, dynamic typing
- Single data type multi dimensional array + sparse array (value + index)
- J concise notation of computation = power for rapid algorithm development
- Tacit programming = point-free style + function composition
- Evaluate right-to-left (no precedence, right-associative), read left-to-right
- Literals: `_1` negative, `_` infinity, `1j2` complex
- Common operations:
  - `NB.` comment
  - `=.` assignment
  - `?` roll with repetition / deal no repetition
- Arithmetic operations:
  - `+` conjugate / addition
  - `-` negate / subtraction
  - `*` signum / multiplication
  - `%` reciprocal / division
  - `%:` square root `sqrt`
  - `^` power `expt`
  - `^.` natural logarithm `log`
  - `10 ^.` decimal logarithm `log <> 10`
  - `<.` floor / minimum
  - `>.` ceiling / maximum
  - `|` absolute value / remainder
  - `>:` increment
  - `<:` decrement
  - `!` factorial / out of
- Array operations:
  - `#` length / copy
  - `{` index
  - `i.` index generator / index of
  - `e.` membership
  - `$` shape of / reshape
  - `,` ravel / append
  - `/` insertion (fold) / table
  - `{.` take
  - `.}` drop
  - `{:` last
  - `,:` laminate
- Constants
  - `a.` alphabet, `[0-9]` 48, `[A-Z]` 65, `[a-z]` 97
- Comparison operations: `<`, `<:`, `>`, `>:`, `=` equal, `~:` not equal
- Logical operations: `*.` and, `+.` or, `-.` not, `"*:` not-and, `+:` not-or
- Noun = data, verb = function, modifier = verb modification
- Monadic verb `v n`
- Diadic verb `n0 v n1`
- Fork `(v0 v1 v2) n = (v0 n) v1 (v2 n)`