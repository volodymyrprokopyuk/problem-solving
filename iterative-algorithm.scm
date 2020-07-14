(define-module
  (iterative-algorithm)
  #:export
  (range-between integer-factorization number-divisors perfect-number?
                 perfect-numbers prime-numbers greatest-common-divisor
                 lowest-common-multiple happy-ticket? happy-tickets))

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

(define greatest-common-divisor
  (case-lambda
    "Finds GCD of all arguments"
    [(a) a]
    [(a b)
     (let gcd* ([a (max a b)] [b (min a b)])
       (if (zero? (remainder a b)) b (gcd* b (remainder a b))))]
    [(a b . r)
     (apply greatest-common-divisor (greatest-common-divisor a b) r)]))

(define lowest-common-multiple
  (case-lambda
    "Finds LCM of all arguments"
    [(a) a]
    [(a b) (/ (* a b) (greatest-common-divisor a b))]
    [(a b . r)
     (apply lowest-common-multiple (lowest-common-multiple a b) r)]))

(define (happy-ticket? tnumber mdigits)
  "Returns #t if the sum of the first mdigits of the tnumber is equal to"
  " the sum of the last mdigits of the tnumber"
  (when (< (string-length tnumber) mdigits)
    (error "happy-ticket?: ticket number too small:" tnumber))
  (let* ([ds (unfold string-null?
                     (lambda (s) (string->number (string-take s 1)))
                     (lambda (s) (string-drop s 1)) tnumber)]
         [first-mdigits (take ds mdigits)]
         [last-mdigits (take-right ds mdigits)]
         [first-mdigits-sum (fold + 0 first-mdigits)]
         [last-mdigits-sum (fold + 0 last-mdigits)])
    (= first-mdigits-sum last-mdigits-sum)))

(define (happy-tickets tlength mdigits)
  (let* ([ns (iota (expt 10 tlength))]
         [ts (map (lambda (n) (string-pad (number->string n) tlength #\0)) ns)])
    (filter (lambda (t) (happy-ticket? t mdigits)) ts)))
