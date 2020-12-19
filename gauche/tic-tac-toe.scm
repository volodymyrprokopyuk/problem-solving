(define-module tic-tac-toe)

(select-module tic-tac-toe)

(use srfi-42)
(use gauche.parseopt)
(use gauche.parameter)

(define-constant name "Tic-tac-toe")
(define-constant description "Noughts and corsses game. To win put three marks in a row \
    (horizontal, vertical or diagonal)")
(define-constant version "0.1.0")
(define-constant author "Volodymyr Prokopyuk")
(define-constant year "2020")

(define-condition-type <app-error> <error> app-error? (message message))
(define-condition-type <input-error> <app-error> input-error?)
(define-condition-type <index-error> <app-error> index-error?)

(define (report-error e)
  "Display the error e message on the stderr"
  (display #"Error: ~(message e)\n" (current-error-port)))

(define (string->mark s)
  "Converts the string s to player mark character either #\X or #\O"
  (when [zero? (string-length s)] (error <input-error> :message "empty mark"))
  (let ([c ($ char-upcase $ string-ref s 0)])
    (if [#[XO] c] c (error <input-error> :message #"invalid mark ~c"))))

(define (validate-strategy s)
  "Validates supported strategies"
  (unless [memq s '(easy hard impossible)]
    (error <input-error> :message #"invalid strategy ~s"))
  s)

(define help? (make-parameter #f))
(define version? (make-parameter #f))
(define player-first (make-parameter #t))
(define player-mark (make-parameter "X" string->mark))
(define strategy (make-parameter 'easy validate-strategy))

(define (configure args)
  "Configures tic-tac-toe by reading command line options"
  (let-args args
    ([h "h|help"]
     [v "v|version"]
     [f "f|first"]
     [m "m|mark=s"]
     [s "s|strategy=y"]
     [else (o . r) (error <input-error> :message #"unknown option ~o\n")])
    (help? h)
    (version? v)
    (player-first (not f))
    (and m (player-mark m))
    (and s (strategy s))))

(define (program-help)
  "Returns program name, description, author, year and command line options"
  #"~name version ~version, ~author, ~year
~description

Usage: tic-tac-toe [--help | --version ] [--mark <m> --strategy <s>]

Options:
  -h, --help          Show help
  -v, --version       Show version
  -f, --first         Program first
  -m, --mark <m>      Set player mark: X (default), O
  -s, --startegy <s>  Select strategy: easy (default), hard, impossible
")

(define (game-help)
  "Returns game commands descirption"
  #"(q)uit, (h)elp, (v)ersion, (r)eset, (1-9) marks\n")

(define (make-board)
  "Creates an empty 9-cells board"
  (make-vector 9 #\_))

(define (mark! b i m)
  "Sets the mark m (X or O) on the board b at the index i"
  (when [memv i (list-ec (:vector c (index j) b) (if [#[XO] c]) j)]
    (error <index-error> :message #"posiiton ~(+ i 1) already used"))
  (set! (~ b i) m))

(define (board->string b)
  "Converts the board b into a string representation with 3-cells rows"
  (string-append-ec
   (:vector c (index i) b)
   (string c (if [zero? (remainder (+ i 1) 3)] #\newline #\space))))

(define (program-mark)
  "Returns program mark opposite to player mark"
  (if [#[X] (player-mark)] #\O #\X))

(define (setup-board)
  "Makes board and makes the first mark if the program hoes first"
  (let ([b (make-board)])
    (cond
      [(player-first) b]
      [else (mark! b 4 (program-mark)) b])))

(define (play)
  (define (prompt) (display "> ") (flush))
  (define (read-command)
    (let ([c (read-char)])
      (cond
        [(or (eof-object? c) (#[^ \n] c)) c]
        [else (read-command)])))
  "Plays tic-tac-toe by reading commands entered by the player"
  (let play* ([b (setup-board)])
    (display (board->string b))
    (prompt)
    (let ([c (read-command)])
      (cond
        [(or (eof-object? c) (#[q] c)) (display "quit\n")]
        [(#[h] c) (display (game-help)) (play* b)]
        [(#[v] c) (display #"version ~version\n") (play* b)]
        [(#[r] c) (play* (setup-board))]
        [(#[1-9] c)
         (let ([i (- (digit->integer c) 1)])
           (mark! b i (player-mark)))
         (play* b)]
        [else (display #"error ~c\n") (play* b)]))))

(define (main args)
  (guard
   (e
    [(<input-error> e) (report-error e) 1])
   (configure (cdr args))
   (cond
     [(help?) (display (program-help))]
     [(version?) (display #"~name version ~version\n")]
     [else (play)])
   0))
