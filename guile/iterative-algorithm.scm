(define-module
  (iterative-algorithm)
  #:export
  (range-between integer-factorization number-divisors perfect-number?
                 perfect-numbers prime-numbers greatest-common-divisor
                 lowest-common-multiple happy-ticket? happy-tickets
                 armstrong-number? armstrong-numbers dec->str str->dec group-by
                 rotate-right rotate-left))

(use-modules
 (srfi srfi-1) ;; List library
 (srfi srfi-69) ;; Hash table
 (srfi srfi-42) ;; Comprehensions
 ((ice-9 pretty-print)
  #:select ((pretty-print . pp)))
 )

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
  "Builds list of happy tickets of tlength with equal mdigits"
  (let* ([ns (iota (expt 10 tlength))]
         [ts (map (lambda (n) (string-pad (number->string n) tlength #\0)) ns)])
    (filter (lambda (t) (happy-ticket? t mdigits)) ts)))

(define (armstrong-number? n)
  "Returns #t if the sum of power n of digits of n equals to n itself"
  (let* ([ds (unfold-right zero?
                           (lambda (nn) (remainder nn 10))
                           (lambda (nn) (quotient nn 10)) n)]
         [ds-sum (fold (lambda (d s) (+ s (expt d (length ds)))) 0 ds)])
    (= n ds-sum)))

(define (armstrong-numbers n)
  "Builds the list of Armstrong numbers less than n"
  (filter armstrong-number? (iota n)))

(define* (dec->str n #:optional (radix 10))
  "Converts the decimal number n into a string representation with the radix"
  (when (> radix 16) (error "dec->str: radix too big:" radix))
  (let ([ds (unfold zero?
                    (lambda (nn) (remainder nn radix))
                    (lambda (nn) (quotient nn radix)) n)]
        [dec->hex
         (alist->hash-table
          '((10 . "a") (11 . "b") (12 . "c") (13 . "d") (14 . "e") (15 . "f")))])
    (fold
     (lambda (d s)
       (string-append
        (if (< d 10)
            (number->string d)
            (hash-table-ref dec->hex d)) s)) "" ds)))

(define* (str->dec s #:optional (radix 10))
  "Converts the string representation s with the radix into a decimal number"
  (when (> radix 16) (error "str->dec: radix too big:" radix))
  (when (string-any
         (char-set-complement
          (char-set-union
           char-set:digit
           (char-set #\a #\b #\c #\d #\e #\f))) (string-downcase s))
    (error "str->dec: invalid digit:" s))
  (let* ([char->digit
          (alist->hash-table
           '((#\0 . 0) (#\1 . 1) (#\2 . 2) (#\3 . 3) (#\4 . 4) (#\5 . 5) (#\6 . 6)
                       (#\7 . 7) (#\8 . 8) (#\9 . 9) (#\a . 10) (#\b . 11) (#\c . 12)
                       (#\d . 13) (#\e . 14) (#\f . 15)))]
         [cs (string-fold cons '() (string-downcase s))]
         [ds (map (lambda (c) (hash-table-ref char->digit c)) cs)]
         [dsp (zip ds (iota (length ds)))])
    (fold (lambda (dp n) (+ n (* (car dp) (expt radix (cadr dp))))) 0 dsp)))

(define (group-by n l)
  "Groups list elements into groups of size n"
  (let group-by* ([l l] [i 0] [g '()] [r '()])
    (cond
      [(null? l) (reverse (cons (reverse g) r))]
      [(< i n) (group-by* (cdr l) (1+ i) (cons (car l) g) r)]
      [else (group-by* (cdr l) 1 (list (car l)) (cons (reverse g) r))])))

(define (rotate-left n l)
  "Rotates left n times the list l"
  (let ([rotate-left-1 (lambda (l) (append (cdr l) (list (car l))))])
    (do ([i 0 (1+ i)] [l l (rotate-left-1 l)])
        ([= i n] l))))

;; (pp (rotate-left 1 (list-ec (:range i 1 5) i)))

(define (rotate-right n l)
  "Rotates right n times the list l"
  (let ([rotate-right-1
         (lambda (l)
           (let* ([r (reverse l)])
             (cons (car r) (reverse (cdr r)))))])
    (do ([i 0 (1+ i)] [l l (rotate-right-1 l)])
        ([= i n] l))))

;; (pp (rotate-right 4 (list-ec (:range i 1 5) i)))

(define* (round-number x #:optional (n 3))
  "Rounds the number x to the n decimal positions"
  (let ([k (expt 10 n)])
    (/ (round (* (exact->inexact x) k)) k)))

(define* (square-root x #:optional (p 0.001))
  "Returns an approximation of the square root of the number x with the precision p"
  (do ([u (/ x 2) v] [v (/ x 3) (/ (+ u (/ x u)) 2)])
      ([< (abs (- u v)) p] (round-number v 6))
    (format #t "~s\n" (round-number v 6))))

;; (pp (square-root 1))
;; (pp (sqrt 1))
;; (pp (square-root 2))
;; (pp (sqrt 2))
;; (pp (square-root 3))
;; (pp (sqrt 3))

(define* (exchange-money x #:optional (bn '(100 50 20 10 5 1)))
  "Exchanges the x amount of money given the bn set of banknotes"
  (let exchange* ([x x] [bn bn] [r '()])
    (if [zero? x] (reverse r)
        (exchange*
         (remainder x (car bn))
         (cdr bn)
         (let ([q (quotient x (car bn))])
           (if [zero? q] r (cons (cons q (car bn)) r)))))))

;; (pp (exchange-money 0))
;; (pp (exchange-money 46))
;; (pp (exchange-money 237))
