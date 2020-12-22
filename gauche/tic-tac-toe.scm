(define-module tic-tac-toe)

(select-module tic-tac-toe)

(use srfi-42) ;; Comprehensions
(use srfi-27) ;; Random
(use gauche.parameter)
(use gauche.parseopt)

;; Random

(random-source-randomize! default-random-source)

;; Constant

(define-constant name "Tic-tac-toe")
(define-constant description "Noughts and corsses game. To win put three marks in a row \
    (horizontal, vertical or diagonal)")
(define-constant version "0.1.0")
(define-constant author "Volodymyr Prokopyuk")
(define-constant year "2020")

;; Error

(define-condition-type <app-error> <error> app-error? (message message))
(define-condition-type <input-error> <app-error> input-error?)

(define (report-error e)
  "Shows the error e message on the stderr"
  (display #"Error: ~(message e)\n" (current-error-port)))

;; Mark

(define (string->mark s)
  "Converts the string s to a mark char-set either #[X] or #[O]"
  (when [zero? (string-length s)] (error <input-error> :message "empty mark"))
  (let ([c ($ char-upcase $ string-ref s 0)])
    (if [#[XO] c] (char-set c) (error <input-error> :message #"invalid mark ~c"))))

(define (mark->char m)
  "Converts the mark m either #[X] or #[O] into a corresponding character"
  (if [m #\X] #\X #\O))

(define (opposite-mark m)
  "Returns #[X] when the mark m is #[O] and viceversa"
  (if [m #\X] (char-set #\O) (char-set #\X)))

(define (program-mark)
  "Returns the opposite to the player mark parameter"
  (opposite-mark (player-mark)))

(define (empty-mark)
  "Returns the empty mark #[_]"
  #[_])

;; Board

(define (make-board)
  "Creates an empty board with 9 empty cells"
  (make-vector 9 #\_))

(define (mark! b i m)
  "Sets the mark m #\X or #\O on the board b at the index i"
  (set! (~ b i) m))

(define (board->string b)
  "Converts the board b into the string representation with 3-cells rows"
  (string-append-ec
   (:vector c (index i) b)
   (string c (if [zero? (remainder (+ i 1) 3)] #\newline #\space))))

(define (setup-board)
  "Makes a board and makes the first mark if the program goes first"
  (let ([b (make-board)])
    (cond
      [(player-first) b]
      [else (mark! b ((program-strategy) b) ($ mark->char $ program-mark)) b])))

;; Check board

(define (cell-marked? b i)
  "Returns #t if the cell at the index i is already marked on the board b"
  (any?-ec (:vector c (index j) b) (and [= i j] (not [(empty-mark) c]))))

(define (row-marked? b r m)
  "Returns #t if the row r is marked with the mark m on the board b"
    (every?-ec (:vector c (index i) b) (if [memv i r]) [m c]))

(define (win? b m)
  "Returns #t if the there is a row on the board b marked with the mark m"
  (scan-rows b (cut row-marked? b <> m)))

(define (complete? b)
  "Returns #t if all the cells the board b are marked"
  (every?-ec (:vector c b) (not [(empty-mark) c])))

(define (check-board b m play*)
  "Checks the board b for the win of the mark m or draw, otherway continues to play*"
  (let* ([player? (equal? m (player-mark))]
         [s (if player? "player" "program")])
    (cond
      [(win? b m) (display (board->string b)) (display #"~s won\n")]
      [(complete? b) (display (board->string b)) (display "draw\n")]
      [player? (program-turn b play*)]
      [else (play* b)])))

;; Strategy

(define (validate-strategy s)
  "Validates supported strategies"
  (unless [memq s '(easy hard impossible)]
    (error <input-error> :message #"invalid strategy ~s"))
  s)

;; Random strategy

(define (random-strategy b)
  "Returns the index of a randomly selected cell from the available empty cells"
  (let* ([e (vector-ec (:vector c (index i) b) (if [(empty-mark) c]) i)]
         [i (random-integer (vector-length e))])
    (vector-ref e i)))

;; Row-based strategy

(define (scan-rows b f)
  "Scans the board b rows (horizontal, vectical, diagonal) with the function f \
   and returns the first non-#f result"
  (any f '((0 1 2) (3 4 5) (6 7 8) (0 3 6) (1 4 7) (2 5 8) (0 4 8) (2 4 6))))

(define (about-to-win b r m)
  "Returns the missing index in the row r for the mark m to win on the board b, \
   otherwise #f"
  (and (= (fold-ec 0 (:vector c (index i) b) (and [memv i r] [m c]) 1 +) 2)
       (first-ec #f (:vector c (index i) b) (and [memv i r] [(empty-mark) c]) i)))

(define (win-strategy b)
  "Returns the index of the program one-cell-missing to win row, otherwise #f"
  (scan-rows b (cut about-to-win b <> (program-mark))))

(define (block-win-strategy b)
  "Returns the index of the player one-cell-missing to win row, otherwise #f"
  (scan-rows b (cut about-to-win b <> (player-mark))))

;; Corner-based strategy

(define (scan-corners b f)
  "Scans the board b corners (cental cross, square corners) with the \
   function f and returns the first non-#f result"
  (any f '((1 7 4 3 5) (0 1 2 5 8) (2 5 8 7 6) (8 7 6 3 0) (6 3 0 1 2))))

(define (two-non-blocked b r m)
  "Returns the index of the corner r with two non-blocked rows for the mark m \
   on the board b, otherwise #f"
  (and (every?-ec (:vector c (index i) b) (if [memv i r]) (not [(opposite-mark m) c]))
       (every?-ec (:vector c (index i) b) (if [= i (caddr r)]) [(empty-mark) c])
       (caddr r)))

(define (fork-strategy b)
  "Returns the index of the corner with two program non-blocked rows"
  (scan-corners b (cut two-non-blocked b <> (program-mark))))

(define (block-fork-strategy b)
  "Returns the index of the corner with two player non-blocked rows"
  (scan-corners b (cut two-non-blocked b <> (player-mark))))

(define (configure-strategy s)
  "Configures the program strategy based on the strategy s command line option"
  (case s
    [(easy) (any-pred win-strategy random-strategy)]
    ((hard) (any-pred win-strategy block-win-strategy random-strategy))
    ((impossible)
     (any-pred win-strategy block-win-strategy fork-strategy block-fork-strategy
               random-strategy))
    [else random-strategy]))

(define (program-strategy)
  "Returns configured program strategy"
  (configure-strategy (strategy)))

;; Help

(define (program-help)
  "Returns the program name, description, author, year and command line option"
  #"~name version ~version, ~author, ~year
~description

Usage: tic-tac-toe [--help | --version ] [--first --mark <m> --strategy <s>]

Options:
  -h, --help          Show help
  -v, --version       Show version
  -f, --first         Program first (player first default)
  -m, --mark <m>      Set player mark: X (default), O
  -s, --startegy <s>  Select strategy: easy (default), hard, impossible
")

(define (game-help)
  "Returns the tic-tac-toe commands descirption"
  #"(q)uit, (h)elp, (v)ersion, (r)eset, (1-9) marks\n")

;; Parameter

(define help? (make-parameter #f))
(define version? (make-parameter #f))
(define player-first (make-parameter #t))
(define player-mark (make-parameter "X" string->mark))
(define strategy (make-parameter 'easy validate-strategy))

;; Configuration

(define (configure args)
  "Configures the tic-tac-toe program from the command line options"
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
    (display #"player mark ~($ mark->char $ player-mark)\n")
    (and s (strategy s))
    (display #"program strategy ~(strategy)\n")))

;; Play

(define (player-turn b c play*)
  "Realizes player's turn"
  (let ([i (- (digit->integer c) 1)])
    (cond
      [(cell-marked? b i) (display #"~(+ i 1) already marked\n") (play* b)]
      [else
       (mark! b i ($ mark->char $ player-mark))
       (check-board b (player-mark) play*)])))

(define (program-turn b play*)
  "Realizes program's turn"
  (let ([i ((program-strategy) b)])
    (mark! b i ($ mark->char $ program-mark))
    (check-board b (program-mark) play*)))

(define (play)
  (define (prompt) (display "> ") (flush))
  (define (read-command)
    (let ([c (read-char)])
      (cond
        [(or (eof-object? c) (#[^ \n] c)) c]
        [else (read-command)])))
  "Plays the tic-tac-toe by reading commands entered by the player"
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

;; Main

(define (main args)
  "Starts program execution"
  (guard
   (e
    [(<input-error> e) (report-error e) 1])
   (configure (cdr args))
   (cond
     [(help?) (display (program-help))]
     [(version?) (display #"~name version ~version\n")]
     [else (play)])
   0))
