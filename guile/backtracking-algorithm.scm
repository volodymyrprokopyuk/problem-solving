(define-module
  (backtracking-algorithm)
  #:export (queen queen-format))

(use-modules
 (srfi srfi-42) ;; Comprehensions
 ((ice-9 pretty-print)
  #:select ((pretty-print . pp))))

(define (queen-legal? q b)
  "Returns #t if the queen position q is legal in the alrady leagal partial board b"
  (let legal* ([b b] [l (1- q)] [r (1+ q)])
    (if [null? b] #t
        (let* ([p (car b)])
          ;; The queen position q is not on the same column with the current position p
          ;; The current position p is not on the left and not on the right diagonals of
          ;; the queen position
          (and (not (= q p)) (not (= l p)) (not (= r p))
               (legal* (cdr b) (1- l) (1+ r)))))))

(define (queen-format b)
  "Formats the board b that represents a solution to the n-queen problem"
  (string-ec (:list q b) (:range i (1+ (length b)))
             (cond [(= i q) #\Q] [(= i (length b)) #\newline] [else #\.])))

(define (queen-format2 b)
  "Formats the board b that represents a solution to the n-queen problem"
  (let ([l (length b)])
    (let format* ([b b] [r '()])
      (if [null? b] (string-join r "\n")
          (do ([i 0 (1+ i)] [rr '() (cons (if [= i (car b)] #\Q #\.) rr)])
              ([= i l] (format* (cdr b) (cons (list->string rr) r))))))))

(define (queen n)

  (define (queen-solution? b)
    "Returns #t if the board b represents a valid n-queen problem solution"
    (= (length b) n))

  (define (queen-extend q b)
    "Incrementally extends the already legal partial solution b with the next queen q"
    (cond
      ;; The complete solution is found
      [(queen-solution? b) (format #t "Solution: ~s\n" b) b]
      ;; All queen positions for the next row are invalid. The current board even
      ;; being a legal partial solution does not lead to any complete solution.
      ;; Backtrack the legal partial solution by moving to the next position in the
      ;; current row
      [(= q -1) (format #t "  # Fail: ~s ~s\n" q b) (queen-backtrack b)]
      ;; A valid queen position in the next row is found. Add the queen position in
      ;; the next row to the board, and further extend the new board with the next row
      ;; (deep-first tree search)
      [(queen-legal? q b)
       (format #t "  > Found: ~s ~s\n" q b)
       (queen-extend (1- n) (cons q b))]
      ;; Check the next queen position in the next row
      [else (format #t "  ? Check: ~s ~s\n" q b) (queen-extend (1- q) b)]))

  (define (queen-backtrack b)
    "Moves to the next position in the current row of the legal partial solution b"
    (format #t "  < Backtrack: ~s\n" b)
    ;; There is no solution at all
    (if [null? b] b
        ;; Check the next position in the current row of a legal partial solution
        (queen-extend (1- (car b)) (cdr b))))

  (define (queen-extend-all)
    "Finds all solutions to the n-queen problem"
    (let extend* ([b (queen-extend (1- n) '())] [r '()])
      (if [null? b] r (extend* (queen-backtrack b) (cons b r)))))

  (queen-extend-all))

(for-each (lambda (b) (pp b) (display (queen-format b)) (newline)) (queen 4))
