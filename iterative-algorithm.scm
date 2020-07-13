(define-module
  (iterative-algorithm)
  #:export (range-between integer-factorization number-divisors perfect-number?
                          perfect-numbers, prime-numbers))

(use-modules
 ((ice-9 pretty-print)
  #:select ((pretty-print . pp)))
 (srfi srfi-1)) ;; List library

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
      [(zero? (remainder n d)) (ifact* (quotient n d) d (cons d r))]
      [else (ifact* n (1+ d) r)])))

(define (number-divisors n)
  "Returns all divisors of the number n excluding the number itself"
  (let ndivs* ([d 2] [r '(1)])
    (cond
      [(>= d n) (reverse r)]
      [(zero? (remainder n d)) (ndivs* (1+ d) (cons d r))]
      [else (ndivs* (1+ d) r)])))

(define (perfect-number? n)
  "Returns #t if the number n equals to the sum of all its divisors including 1"
  " and excluding itself"
  (let* ([ds (number-divisors n)]
         [s (fold + 0 ds)])
    (= n s)))

(define (perfect-numbers n)
  "Buils the list of perfect numbers not greater than n"
  (filter perfect-number? (iota n 2)))

(define (prime-numbers n)
  "Returns the list of prime numbers using the Sieve of Eratosthenes algorithm"
  (let prime* ([p 2] [l (iota (1- n) 2)] [r '()])
    (if p
        (prime*
         ;; Find the next prime
         (find (lambda (e) (> e p)) l)
         ;; Remove multiples of the next prime
         (remove (lambda (e) (zero? (remainder e p))) l)
         ;; Add the current prime to the result
         (cons p r))
        (reverse r))))
