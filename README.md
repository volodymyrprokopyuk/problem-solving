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
- Tail-call optimizaiton + recursion (`let name`)
- First-class continuations (`call/cc`) for (non-local exit, exceptions, generators,
  coroutines, backtracking, actors)
- Delayed evaluation (`delay`, `force`) + promises (memoization)
- Hygienic macros (`define-syntax`, `syntax-rules`) + recursive macro expansion
- Module system
    - Definition `define-module`, `select-module`, `export (rename)`,
      `import :only :except :rename :prefix`, `extend`
    - Usage `use`, `with-module`
- Object system
    - Class `define-class :allocation (:instance :class :virtual [:slot-ref :slot-set!])
      :init-keyword :init-value :init-form :accessor :getter :setter`
    - Instance `make`
    - Method `define-method`, `next-method`
- Exceptions and conditions
    - Conditions `define-condition-type`
    - Signaling `error`, `raise`
    - Handling `guard`
    - Cleaning up `unwind-protect`
- Fundamental and derived forms
  - `lambda` + `:optional`, `:key`, `:rest` (procedure, closure, binding block,
    recursion)
    - `case-lambda` procedure with variable number of arguments
    - `let` (block), `let*` (nested), `letrec` (set!), `let name` (recursion)
    - `and-let*` sequential `*` guarded `and` binded `let` expressions
    - `values` + `receive` multiple values construction and access
    - `cut` compact parameter spacialization without currying
    - `$`, `$*` procedure application chaining
    - `.$` procedure composition
  - `define` (variable definition + initialization), `set!` (variable update +
    assignment)
    - generalized `set!`
    - universal accessor `~`
  - `if` (conditionals)
    - `and`, `or`, `cond`, `case`, `when`, `unless`
  - `quote`=`'` (do not evaluate)
    - `quasiquote`=`` ` ``, `unquote`=`,`, `unquote-splicing`=`,@`
  - `define-syntax`, `let-syntax`, `letrec-syntax` (form declaration)
  - `syntax-rules` (pattern-template extension), `syntax-case` (precedural extension)
  - Sequencing + side effects
    - `begin`
  - Iterator + side effects
    - `do`
  - Delayed (lazy) evaluation + promises
    - `delay` e -> promise e, `force` promise e -> e, `lazy` promise e -> promise e,
      `eager`
- Literals `#t`, `#f`, `#\char`, `(list)`, `#(vector)`, `#[char-set]`, `#/reg-exp/`
- String interpolation `#"Value ~expr ~(expr) ~|var|"`
- Parameters `make-parameter`, `parametrize`, `dynamic-wind` (dynamic environment
  management, context switch)
- Pattern matching `match`, `match-lambda` (one argument or list of arguments),
  `match-let`
- Eager comprehensions `(comprehension qualifiers body)`
    - Comprehension (collect, aggregate/fold) `do-ec`, `list-ec`, `vector-ec`,
      `string-ec`, `any?-ec`, `every?-ec`, `first-ec`, `last-ec`, `fold-ec`
    - Qualifiers (generate, filter)
        - Generational (typed) qualifiers `:list`, `:vector`, `:string`, `:integers`
          (infinite), `:rage`, `:real-range`, `:char-range`, `:port`, `:parallel` (zip,
          default is nested), `:while` (stop early)
        - Control qualifiers `if`, `not`, `and`, `or`
    - Body (evaluate, transform)
- Record type (portable and efficient class `<record>`) `define-record-type`
  (constructor, predicate, accessors, [mutators])

## Equality, comparison/ordering, and hashing

- Equality
  - `eq?` symbol, boolean, reference
  - `eqv?` number `=`, character `char=?`
  - `equal?` aggregate, recursive, `string=?`
    - `object-equal?` generic function for user-defined data types (UDDT)
- Comparison and ordering
  - `compare` -1, 0, 1, `<`, `char<?`, `string<?`
    - `object-compare` generic function for UDDT
- Hasing
  - `default-hash`, `portable-hash`
    - `object-hash` generic function for UDDT
- Comparator
  - Equality, comparison/ordering, and hasing procedure abstraction under common
    interface
  - `comparator-test-type` type check `type?`
  - `=?` equality `equal?`
  - `<?`, `<=?`, `>?`, `>=?` ordering `<` or `compare`
  - `comparator-compare` comparison `compare`
  - `comparator-hash` hasing `default-hash`
  - `default-comparator #t equal? compare default-hash` automatically extended for UDDT
    via `object-equal?`, `object-compare`, and `object-hash`
