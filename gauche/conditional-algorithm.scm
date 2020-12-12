(use util.match)

(define analyze-let
  (match-lambda
    [('let (? symbol?) ((var expr) ...) body ...)
     (format "named let, vars=~s, exprs=~s" var expr)]
    [('let ((var expr) ...) body ...)
     (format "normal let, vars=~s, exprs=~s" var expr)]
    [_ (format "malformed let")]))

;; #?=(analyze-let '(let ([a 1] [b 2]) c d))
;; #?=(analyze-let '(let name ([a '(1 2)] [b '(3 4)]) c d))
;; #?=(analyze-let '(let (a) b))

(define extract-file-name
  (match-lambda
    [(? string? (= #/^(.+)\.([^.]+)$/ m))
     (format "base=~a, extension=~a" (m 1) (m 2))]))

;; #?=(extract-file-name "conditional-algorithm.scm")

(define quasipattern
  (match-lambda
    [`(the value is ,value) value]))

;; #?=(quasipattern '(the value is 43))

(define extract-pair
  (match-lambda
    [(and p (a . b)) (values p a b)]))

;; #?=(extract-pair (cons 1 2))

(define (european-hour? h)
  "Returns #t if the hour h is within [0, 24]"
  (<= 0 h 24))

;; Converts the european time to the american time notation
(define european->american
  (match-lambda
    [(? european-hour? h)
     (cond
       [(zero? (remainder h 24)) '(12 am midnight)]
       [(zero? (remainder h 12)) '(12 pm noon)]
       [(and (< h 12)) `(,(remainder h 12) am)]
       [else `(,(remainder h 12) pm)])]))

;; (for-each (lambda (t) #?=(european->american t)) '(0 1 11 12 13 23 24))

(define (american-hour? h)
  "Returns #t if the hour h is within [1, 12]"
  (<= 1 h 12))

;; Converst the american time to the european time notation
(define american->european
  (match-lambda
    [((? american-hour? h) (and t (or 'am 'pm)))
     (cond
       [(and (= h 12) (eq? t 'am)) 0]
       [(eq? t 'am) h]
       [(= h 12) h]
       [else (+ h 12)])]))

;; (for-each (lambda (t) #?=(american->european t))
;;           '((12 am) (1 am) (11 am) (12 pm) (1 pm) (11 pm) (12 am)))
