(define-module
  (iterative-algorithm)
  #:export (range-between integer-factorization))

(use-modules
 ((ice-9 pretty-print)
  #:select ((pretty-print . pp))))

(define (range-between a b)
  "Builds range of consecutive numbers between a and b both inclusive"
  (when (> a b) (error "range-between: empty range:" a b))
  (do ([i a (1+ i)] [r '() (cons i r)])
      ([> i b] (reverse r))))

(define (integer-factorization n)
  "Represents the composite integer n as a product of smaller prime numbers"
  (when (< n 2) (error "integer-factorization: smaller than 2 number:" n))
  (let ifact* ([n n] [d 2] [r '()])
    (cond
      [(< n d) (reverse r)]
      [(zero? (remainder n d))
       (ifact* (quotient n d) d (cons d r))]
      [else (ifact* n (1+ d) r)])))
