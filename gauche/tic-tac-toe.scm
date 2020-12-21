(define-module tic-tac-toe)

(select-module tic-tac-toe)

(use srfi-42) ;; Comprehensions
(use srfi-27) ;; Random
(use gauche.parameter)
(use gauche.parseopt)

(random-source-randomize! default-random-source)

(define-constant name "Tic-tac-toe")
(define-constant description "Noughts and corsses game. To win put three marks in a row \
    (horizontal, vertical or diagonal)")
(define-constant version "0.1.0")
(define-constant author "Volodymyr Prokopyuk")
(define-constant year "2020")

(define-condition-type <app-error> <error> app-error? (message message))
(define-condition-type <input-error> <app-error> input-error?)

(define (report-error e)
  "Display the error e message on the stderr"
  (display #"Error: ~(message e)\n" (current-error-port)))

(define (random-strategy b)
  "Realizes random program strategy"
  (let* ([a (vector-ec (:vector c (index i) b) (if [#[_] c]) i)]
         [i (random-integer (vector-length a))])
    (vector-ref a i)))

(define (scan-board b f)
  "Scans the board b rows with the function f and returns the first non-#f result"
  (any f '((0 1 2) (3 4 5) (6 7 8) (0 3 6) (1 4 7) (2 5 8) (0 4 8) (2 4 6))))

(define (about-to-win b r m)
  "Returns the missing index in the row r for the mark m to win on the board b, \
   otherwise #f"
  (and (= (fold-ec 0 (:vector c (index i) b) (and (memv i r) (char=? c m)) 1 +) 2)
       (first-ec #f (:vector c (index i) b) (and [memv i r] [#[_] c]) i)))

(define (win-strategy b)
  "Relizes win program strategy if possible, otherwise returns #f"
  (scan-board b (cut about-to-win b <> (program-mark))))

(define (configure-strategy s)
  "Configures program strategy based on the strategy s command line option"
  (case s
    [(easy) random-strategy]
    ((hard) (any-pred win-strategy random-strategy))
    [else random-strategy]))

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
(define program-strategy (make-parameter (configure-strategy (strategy))))

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
    (display #"player mark ~(player-mark)\n")
    (and s (strategy s))
    (display #"program strategy ~(strategy)\n")
    (program-strategy (configure-strategy (strategy)))))

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
      [else (mark! b ((program-strategy) b) (program-mark)) b])))

(define (cell-marked? b i)
  "Returns #t if the cell at the index i is already marked on the board b"
  (any?-ec (:vector c (index j) b) (and [#[XO] c] [= i j])))

(define (row-marked? b r m)
  "Returns #t if the row r is marked with the mark m on the board b"
    (every?-ec (:vector c (index i) b) (if (memv i r)) [char=? c m]))

(define (win? b m)
  "Returns #t if the there is a row on the board b marked with the mark m"
  (scan-board b (cut row-marked? b <> m)))

(define (complete? b)
  "Returns #t if the board b is complete"
  (every?-ec (:vector c b) [#[^_] c]))

(define (check-board b m play*)
  "Checks the board b for winner or draw the the mark m, otherway continues to play*"
  (let* ([player? (char=? m (player-mark))]
         [s (if player? "player" "program")])
    (cond
      [(win? b m) (display (board->string b)) (display #"~s won\n")]
      [(complete? b) (display (board->string b)) (display "draw\n")]
      [player? (program-turn b play*)]
      [else (play* b)])))

(define (player-turn b c play*)
  "Realizes player's turn"
  (let ([i (- (digit->integer c) 1)])
    (cond
      [(cell-marked? b i) (display #"~(+ i 1) already marked\n") (play* b)]
      [else (mark! b i (player-mark)) (check-board b (player-mark) play*)])))

(define (program-turn b play*)
  "Realizes program's turn"
  (let ([i ((program-strategy) b)])
    (mark! b i (program-mark)) (check-board b (program-mark) play*)))

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
        [(#[1-9] c) (player-turn b c play*)]
        [else (display #"error ~c\n") (play* b)]))))

(define (main args)
  #;(let ([b (make-board)])
    (mark! b 2 #\X)
    (mark! b 8 #\X)
    (display (board->string b))
    #?=(win-strategy b))
  (guard
   (e
    [(<input-error> e) (report-error e) 1])
   (configure (cdr args))
   (cond
     [(help?) (display (program-help))]
     [(version?) (display #"~name version ~version\n")]
     [else (play)])
   0))
