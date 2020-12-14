(define (repeat f n)
  "Composes the function f n times"
  (cond
    [(zero? n) identity]
    [(= n 1) f]
    [else (lambda (x) (f ((repeat f (- n 1)) x)))]))

;; #?=((repeat cdr 3) '(a b c d e f))
;; #?=((repeat (cut + <> 1) 4) 0)
;; #?=((repeat cdr 0) '(a b c d e f))
;; #?=((repeat (cut + <> 1) 0) 0)

;; #?=(map identity '(a b c d))
;; #?=(filter (lambda _ #t) '(a b c d))
;; #?=(fold (lambda (e s) (append s (list e))) '() '(a b c d))
;; #?=(fold-right cons '() '(a b c d))

(define (=map f l)
  (fold-right (lambda (e s) (cons (f e) s)) '() l))

;; #?=(=map identity '(a b c d))

(define (=filter p l)
  (fold-right (lambda (e s) (if [p e] (cons e s) s)) '() l))

;; #?=(=filter (lambda _ #t) '(a b c d))
