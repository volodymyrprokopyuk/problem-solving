# Problem solving

- Data structures + algorithms
- Mutation + objects
- Macros + langauge extension
- Delayed evaluation + promises
- Continuations + control

# Gauche Scheme

- Small core + powerful tools for language extension
- Nested block (`let`) structure + shared namespace for variables and procedures
- Lexcal scope + closures
- Tail-call optimizaiton + recursion (`let name`)
- First-class continuations (`call/cc`) for (non-local exit, exceptions, generators,
  coroutines, backtracking, actors)
- Delayed evaluation (`delay`, `force`, `lazy`, `eager`) + promises (memoization)
- Hygienic macros (`define-syntax`, `syntax-rules`) + recursive macro expansion
- Module system
  - Definition `define-module`, `select-module`, `export (rename)`,
    `import :only :except :rename :prefix`, `extend`
  - Usage `use`, `with-module`
- Object system
  - Class `define-class :allocation (:instance :class :virtual [:slot-ref :slot-set!])
    :init-keyword :init-value :init-form :accessor :getter :setter`, `initialize`,
    `write-object`
  - Instance `make`
  - Method `define-method`, `next-method`
- Exceptions and conditions
  - Conditions `define-condition-type` condition, parent, predicate, slots
  - Signaling `error`, `errorf` simple error, `raise`, `condition` compound condition
  - Handling `guard`
  - Cleaning up `unwind-protect` only calls `cleanup` on normal exit or exception,
    ignors continuation control escapes
  - Control flow `dynamic-wind` always calls `before` and `after` on any control flow
    transition. Low-level management of exceptions, parameters, continuations and ports
    - ```scheme
(define-condition-type <app-error> <error> app-error? [reason reason])

(guard
 (e
  [(<app-error> e)
   (format #t "ERROR: <app-error> ~a ~a" (reason e) (condition-message e))]
  [else (format #t "ERROR: ~a" e)])
 (error "Message")
 (error <app-error> :reason "Reason" "Message")
 (raise (condition [<app-error> (reason "Reason") (message "Message")])))
    ```
- Fundamental and derived forms
  - `lambda` + `:optional`, `:key`, `:rest` (procedure (primitieve, closure), binding
    block, recursion)
    - `case-lambda` procedure with variable number of arguments
    - `let` (block), `let*` (nested), `letrec` (set!), `let name` (recursion)
    - `and-let*` sequential `*` guarded `and` binded `let` expressions
    - `values` + `receive` multiple values construction and access
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
  - First-class objects
    - `call/cc` (continuation),
    - `with-input-from-file`, `with-output-to-file` (port)
    - (stream)
- Literals `#t`, `#f`, `#\char`, `"string"`, `(car . cdr)` (pair), `(list)`,
  `#(vector)`, `#[char-set]`, `#/reg-exp/`
- String interpolation `#"Value ~expr ~(expr) ~|var|"`
- Parameters `make-parameter`, `parametrize`, `dynamic-wind` (dynamic environment
  management, context switch)
- Pattern matching `match`, `match-lambda` (one argument or list of arguments),
  `match-let`
- Record type (portable and efficient class `<record>`) `define-record-type`
  (constructor, predicate, accessors, [mutators])
- Combinators (return procedure)
  - `cut` compact parameter spacialization without currying macro
  - `.$`, `compose` procedure composition
  - `idenitty`, `constantly`, `complement`, `any-pred`, `every-pred`
  - `$`, `$*` procedure application chaining macro

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

## Collections and sequences

- **Collection** = unordered set of objects. Collection provides a set of generic
  functions that iterate over various collecitons (list, vector, string, hash table,
  user-defined class) using the method dispatch of the object system (CLOS)
  - `(use gauche.collection)`
- **Sequence** = ordered set of objects built on top of collection. Sequence is
  accesible through an index and provides order-aware operations on top of collection
  - `(use gauche.sequence)`

## Eager comprehensions

- Eager comprehensions `(comprehension qualifiers body)`
  - (3) Comprehension-ec (collect, aggregate)
    - Collect `do-ec`, `list-ec`, `vector-ec`, `string-ec`
      - Side effects `(do-ec (:list i '(1 2 3)) (display i))`
      - List `(list-ec (:list i '(1 2 3)) i)`
      - String `(string-ec (:list i '(#\a #\b #\c)) i)`
    - Aggregate `any?-ec`, `every?-ec`, `first-ec`, `last-ec`, `fold-ec`
      - Any (short circuit on first #t) `(any?-ec (:list i '(1 2 3)) (even? i))`
      - Every (short circuit on first #f) `(every?-ec (:list i '(1 2 3)) (even? i))`
      - First (short circuit) `(first-ec 0 (:list i '(1 2 3)) i)`
      - Last `(last-ec 0 (:list i '(1 2 3)) i)`
      - Fold `(fold-ec 0 (:list i '(1 2 3)) i +)`
  - (1) Qualifiers (generate, filter)
    - Generational (typed) qualifiers = :generators `:list`, `:vector`, `:string`,
      `:integers` (infinite), `:rage`, `:real-range`, `:char-range`, `:port`,
      `:parallel` (zip, default is nested), `:while` (stop early), `:let` (introduce
      intermediary variables depending on the outer scope)
      - List append `(list-ec (:list i '(1 2 3) '(4 5)) i)`
      - Index `(list-ec (:list i (index j) '(a b c)) (cons i j))`
      - Range `(list-ec (:range i 1 7 2) i)`
      - Port `(with-input-from-string "Vlad"
  (lambda () (list-ec (:port i (current-input-port) read-char) i)))`
      - Nested = cartesian product (the rightmost generator spins fastest)
        `(list-ec (:list i '(1 2)) (:list j '(a b)) (cons i j))`
      - Zip `(:parallel (:list i '(1 2)) (:list j '(a b)))`
      - While from infinite `(list-ec (:while (:integers i) (< i 5)) i)`
      - Let intermediary variable between generators
        `(list-ec (:list i '(1 2 3)) (:let j (* i 10)) (cons i j))`
    - Control qualifiers `if`, `not`, `and`, `or`, `begin` (side effects)
      - `(list-ec (:list i '(1 2 3 4 5 6)) (if [even? i]) i)`
      - `(list-ec (:list i '(1 2 3 4 5 6)) (not [even? i]) i)`
      - `(list-ec (:list i '(1 2 3 4 5 6)) (and [even? i] [< i 5]) i)`
      - `(list-ec (:list i '(1 2 3 4 5 6)) (or [even? i] [odd? i]) i)`
      - Begin side effects between generators
        `(list-ec (:list i '(1 2 3 4 5 6)) (begin (display i)) i)`
  - (2) Body (evaluate, transform)

## Delayed (lazy) evaluation + promises

- `delay` e -> promise e, creates a promise, requires unbound memory for tail-recursive
  algorithms (R5RS)
- `lazy` promise e -> promise e, creates a promie for space-efficient tail-recursive
  lazy algorithms. Generally `lazy` surrounds the entire body of a function expressing
  lazy algorithm (SRFI-45)
- `force` promise e -> e + memoization
- `eager` e -> promise e, eagerly evaluated type converter to a promise
- **Generators** = a procedure with no arguments that yields a series of values ending
  with EOF (very lightweight implementation of on-demand calculations). Generators work
  in a pipeline (DAG) of generators representing a lazy value-propagation network
  - `(use gauche.generator)`
- **Lazy sequence** = indistinguishable from ordinary list structure (all list
  procedures can be used on a lazy sequence) with a lazy pair, whose `car` is
  immediately / eagerly evaluated and whose `cdr` is implicitly / automatically forced
  on demand. Lazy sequence is not strictly lazy and always evaluates one item
  ahead. Lazy sequences are built on top of generators
  - `generator->lseq` efficient lazy sequence, `lcons` makes a thunk / closure for each
    item, `lrange`, `liota`
  - `(use gauche.lazy)`, `lunfold`, `lmap`, `lappend`, `lfilter`, `ltake`, `ltake-while`
- **Streams** = strictly lazy (both `car` and `cdr` are lazily evaluated when aboslutely
  needed) data structure with spacial procedures
  - `(use util.stream)`

## Input and output

- Current ports `current-input-port`, `current-output-port`, `current-error-port`
- Standard ports `standard-input-port`, `standard-output-port`, `standard-error-port`
- File I/O `with-input-from-file`, `with-output-to-file`
- String I/O `with-input-from-string`, `with-output-to-string`, `with-string-io`
- Input `read` s-expression, `read-char`, `read-line`, `port->string`, `eof-object?`
- Output `write`, `write-char`, `write-string`, `write-object` machine, `display`,
  `print`, `pprint` human, `format`, `flush`

# PostgreSQL

- Query
  - `WITH [RECURSIVE] _ AS` named subquery can be used in other named subqueries
  - `SELECT DISTINCT ON (...)` computed value, column alias `c`, `agg_func(DISTINCT
    ... ORDER BY ...)`
  - `FROM` table alias `t`, `t(c)`
    - `JOIN` restrict both relations
    - `LEFT JOIN`, `RIGHT JOIN` keep one relation and enrich with the other if
      matches, otherwise `NULL`
    - `FULL JOIN` keep both relations
    - `CROSS JOIN`=`t1, t2` Cartesian product
    - Join conditions `ON expression`, `USING (list)`
    - `JOIN LATERAL (SELECT ... WHERE ...)` restrict subquery to the current row in
      `FROM` `ON true`. Run the subquery in a loop for each row in `FROM`. Push down
      the join condition into the subquery
  - `WHERE` no alias, `AND`, `OR`, `NOT`, `IN (...)`, `EXISTS (SELECT 1 ...)`
    - Anti-join `WHERE NOT EXISTS (SELECT 1 ...)`
    - Row comparator/assignment `(a, b) = (c, d)`
  - `GROUP BY` alias
    - `GROUPING SETS ((...), ())`=`GROUP` data separately `BY` each `GROUPING SET` and
      then `UNION` with appropriate `NULL`. Aggregate over more than one group at the
      same time in a single query scan
    - `ROLLUP` all prefixes for hierarchical data analysis
    - `CUBE` all subsets (power set)
  - `HAVING` no alias
  - `WINDOW _ AS (PARTITION BY ... ORDER BY ... ROWS PRECEDING | CURRENT | FOLLOWING
    EXCLUDE)` for each input row a frame of peer rows sharing a common property is
    available. Different `OVER` definitions can be used in the same query even with
    `GROUP BY`
  - `ORDER BY` alias `ASC | DESC`
  - `LIMIT n` never use `OFFSET m` use `FETCH` cursor instead
- Set operations `UNION [ALL]`, `INTERSECT`, `EXCEPT` combine query result sets
- Three-valuded logic `TRUE`, `FALSE`, `NULL` + `=`, `<>`
  - `IS DISTINCT FROM` two-valued logic with `NULL`
- Conditional `CASE _ WHEN _ THEN _ ELSE _ END`
- Aggregate function `count(*) FILTER (WHERE ...)`
  - `CREATE AGGREGATE <agg> (type, ...) (
       SFUNC = state transition function (previous state, current row arguments) new state
       STYPE = state value data type
       INITCOND = state initial value
       FINALFUNC = transform the final state value into the aggregate output value)`
- `WITH ... INSERT INTO ... VALUES | SELECT (JOIN ...) ON CONFLICT ... DO NOTHING | DO
  UPDATE SET ... EXCLUDED RETURNING *`
- `WITH ... UPDATE ... SET ... FROM (JOIN ...) WHERE ... RETURNING *`
- `WITH ... DELETE FROM ... USING (JOIN ...) WHERE ... RETURNING *`
- `TRUNCATE ...`
- **Normalization**. Reduces data redundacy, improves data consistency, allows to extend
  the data model without changing existing tables (DDL)
  - **Anomalies**
    - **Update anomaly**. Multiple rows has to be updated with the same data
    - **Insertion anomaly**. More than necessary data has to be inserted
    - **Deletion anomaly**. More than necessary data has to be deleted
  - **Normalization forms** (split tables using identity PK, many-to-one FK and
    many-to-many pivot table)
    - **1NF**. Every attribute value must be atomic
    - **2NF**. Every non-candidate key attribute must depend on the whole candidate key
    - **3NF**. Transtivie dependencies between attributes must be removed
- **Transactions**. Lower isolaiton, fewer locks, more concurrency, more
  phenomena. Highter isolation, more locks, less concurrency, less phenomena (TCL, DML)
  - **Phenomena**
    - **Dirty read**. A transaction reads data written by a concurrent uncommitted
      transaction
    - **Nonrepeatable read**. A transaction re-reads data and finds that data has been
      updated by another transaction that commited after the initial read (`UPDATE` +
      `COMMIT`)
    - **Phantom read**. A transaction re-reads data and finds that data has been
      inserted or deleted by another transaction that commited after the initial read
      (`INSERT`, `DELETE` + `COMMIT`)
    - **Serialization anomaly**
  - **Isolation levels**
    - **Read committed**
    - **Repeatable read**
    - **Serializable**
