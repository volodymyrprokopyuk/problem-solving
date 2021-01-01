(define-module spell-number)

(select-module spell-number)

(use scheme.list) ;; drop-while

(define (number->digits x)
  "Returns a list of digits that represent the number x"
  (let split* ([x x] [e ($ floor $ log x 10)] [d '()])
    (cond
      [(zero? e) ($ reverse $ cons (inexact->exact x) d)]
      [else
       (receive (q r) (quotient&remainder x (expt 10 e))
         (split* r (- e 1) (cons (inexact->exact q) d)))])))

;; #?=(number->digits 123450)
;; #?=(number->digits 9800706)

(define (list->non-overlapped l n)
  "Returns a list of non-overlapped sublists of length n of the list l"
  (let non-overlap* ([l (reverse l)] [r '()])
    (cond
      [(< (length l) (+ n 1)) (cons (reverse l) r)]
      [else (non-overlap* (drop l n) (cons ($ reverse $ take l n) r))])))

;; #?=(list->non-overlapped '(a b c d e f g h) 3)

(define r0-9
  (alist->hash-table
   '((0 . zero) (1 . one) (2 . two) (3 . three) (4 . four) (5 . five) (6 . six)
                (7 . seven) (8 . eight) (9 . nine)) 'eqv?))

(define r10-19
  (alist->hash-table
   '((0 . ten) (1 . eleven) (2 . twelve) (3 . thirteen) (4 . fourteen)
               (5 . fifteen) (6 . sixteen) (7 . seventeen) (8 . eightteen)
               (9 . nineteen)) 'eqv?))

(define r20-90
  (alist->hash-table
   '((2 . twenty) (3 . thirty) (4 . forty) (5 . fifty) (6 . sixty) (7 . seventy)
                  (8 . eighty) (9 . ninety)) 'eqv?))

(define (spell-3-group l)
  (define (spell-2-group l)
    "Spells 2-digit group"
    (let ([l (drop-while zero? l)])
      (case (length l)
        [(1) (list (~ r0-9 (car l)))]
        [(2)
         (cond
           [(= (car l) 1) (list (~ r10-19 (cadr l)))]
           [(zero? (cadr l)) (list (~ r20-90 (car l)))]
           [else (list (~ r20-90 (car l)) (~ r0-9 (cadr l)))])]
        [else '()])))
  "Spells 3-digit group"
  (let ([l (drop-while zero? l)])
    (case (length l)
      [(1) (list (~ r0-9 (car l)))]
      [(2) (spell-2-group l)]
      [(3) (append (list (~ r0-9 (car l)) 'hundred) (spell-2-group (cdr l)))]
      [else '()])))

;; #?=(spell-3-group '(0 0 0))
;; #?=(spell-3-group '(0 0 9))
;; #?=(spell-3-group '(0 1 9))
;; #?=(spell-3-group '(0 9 0))
;; #?=(spell-3-group '(0 9 9))
;; #?=(spell-3-group '(9 0 0))
;; #?=(spell-3-group '(9 0 9))
;; #?=(spell-3-group '(9 1 9))
;; #?=(spell-3-group '(9 9 0))
;; #?=(spell-3-group '(9 9 9))

(define gmagnitude
  (alist->hash-table
   '((0 . unit) (1 . thousand) (2 . million) (3 . billion) (4 . trillion)) 'eqv?))

(define (spell-number x)
  (define (add-magnitude g i)
    "Adds a magnitude to the groupg based on the index i"
    (let ([sg (spell-3-group g)])
      (cond
        [(null? sg) '()]
        [else (append sg (if [zero? i] '() (list (~ gmagnitude i))))])))
  "Spells the number x"
  (let ([g (reverse (list->non-overlapped (number->digits x) 3))])
    (when [> (length g) (hash-table-size gmagnitude)] (error #"too big number ~x"))
    (let spell* ([g g] [i 0] [r '()])
      (cond
        [(null? g) (remove null? r)]
        [else (spell* (cdr g) (+ i 1) (cons (add-magnitude (car g) i) r))]))))

(define (main args)
  "Starts program execution"
  (guard
   (e [else (display #"ERROR: ~e\n" (current-error-port)) 1])
   (pprint (spell-number 1234567890))
   0))
