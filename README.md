# Problem solving

- Data structures + algorithms
- Mutation + objects
- Macros + langauge extension
- Delayed evaluation + promises
- Continuations + control

# Scheme langauge

- Small core + powerful tools for language extension
- Nested block (`let`) structure + shared namespace for variables and procedures
- Lexcal scope + closures
- Tail-call optimizaiton + recursion (`named let`)
- First-class continuations (`call/cc`) for (non-local exit, exceptions, generators,
  coroutines, backtracking, actors)
- Delayed evaluation (`delay`, `force`) + promises (memoization)
- Hygienic macros (`define-syntax`, `syntax-rules`) + recursive macro expansion
- Fundamental and derived forms
  - `lambda` (procedure, closure, binding block, recursion)
    - `let` (block), `let*` (nested), `letrec` (set!), `named let` (recursion)
  - `define` (variable definition + initialization), `set!` (variable update + assignment)
  - `if` (conditionals)
    - `and`, `or`, `cond`, `case`
  - `quote`=`'` (do not evaluate)
    - `quasiquote`=`` ` ``, `unquote`=`,`, `unquote-splicing`=`,@`
  - `define-syntax`, `let-syntax`, `letrec-syntax` (form declaration)
  - `syntax-rules` (pattern-template extension), `syntax-case` (precedural extension)
  - Sequencing + side effects
    - `begin`
  - Iterator + side effects
    - `do`
  - Delayed (lazy) evaluation + promises
    - `delay`
