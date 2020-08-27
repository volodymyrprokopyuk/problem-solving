(define-module
  (abstraction-algorithm)
  #:export
  ;; FRACTION - EXACT ARITHMETIC
  (make-rat numer denom rat-zero? rat-positive? rat-negative? rat+ rat-opposite
            rat- rat* rat-inverse rat/ rat= rat< rat> rat-min mat-max rat-abs
            ;; POLYNOMIAL - SYMBOLIC ALGEBRA
            ))

(use-modules
 (srfi srfi-1) ;; List library
 ((ice-9 pretty-print)
  #:select ((pretty-print . pp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FRACTION - EXACT ARITHMETIC

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data abstraction primitives: constructor and selectors
;; do not expose internal representation

(define (make-rat n d)
  "Makes a rational number from the numerator n and the denominator d"
  (when [zero? d] (error "make-rat: zero denominator"))
  (let ([cd (gcd n d)])
    (if [> cd 1] (cons (/ n cd) (/ d cd)) (cons n d))))

;; (pp (make-rat 1 2))
;; (pp (make-rat 2 4))

(define (numer r)
  "Returns the numerator of the rational r"
  (car r))

(define (denom r)
  "Returns the denominator of the rational r"
  (cdr r))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data abstraction algorithms: do not depend on concrete internal data representation

(define (rat-zero? r)
  "Returns #t if the rational r is zero"
  (zero? (numer r)))

;; (pp (rat-zero? (make-rat 0 1)))

(define (rat-positive? r)
  "Returns #t if the rational r is positive"
  (let ([n (numer r)] [d (denom r)])
    (or (and (positive? n) (positive? d)) (and (negative? n) (negative? d)))))

;; (pp (rat-positive? (make-rat -1 -2)))
;; (pp (rat-positive? (make-rat 0 1)))

(define (rat-negative? r)
  "Returns #t if the rational is negative"
  (let ([n (numer r)] [d (denom r)])
    (or (and (positive? n) (negative? d)) (and (negative? n) (positive? d)))))

;; (pp (rat-negative? (make-rat 1 -2)))
;; (pp (rat-negative? (make-rat 0 1)))

(define (rat+ x y)
  "Adds the rationals x and y"
  (let ([nx (numer x)] [dx (denom x)] [ny (numer y)] [dy (denom y)])
    (make-rat (+ (* nx dy) (* ny dx)) (* dx dy))))

;; (pp (rat+ (make-rat 1 2) (make-rat 1 3)))

(define (rat-opposite r)
  "Returns the opposite for the rational r"
  (make-rat (* -1 (numer r)) (denom r)))

(define (rat- x y)
  "Subtracts the rational y from the rational x"
  (rat+ x (rat-opposite y)))

;; (pp (rat- (make-rat 1 2) (make-rat 1 3)))

(define (rat* x y)
  "Multiplicates the rationals x and y"
  (let ([nx (numer x)] [dx (denom x)] [ny (numer y)] [dy (denom y)])
    (make-rat (* nx ny) (* dx dy))))

;; (pp (rat* (make-rat 1 2) (make-rat 1 3)))

(define (rat-inverse r)
  "Return the inverse for the rational r"
  (when [rat-zero? r] (error "rat-inverse: zero rational"))
  (make-rat (denom r) (numer r)))

(define (rat/ x y)
  "Divides the rational x by the rational y"
  (rat* x (rat-inverse y)))

;; (pp (rat/ (make-rat 1 2) (make-rat 1 3)))

(define (rat= x y)
  "Returns #t if the rationals x and y are equal"
  (let ([nx (numer x)] [dx (denom x)] [ny (numer y)] [dy (denom y)])
    (= (* nx dy) (* dx ny))))

;; (pp (rat= (make-rat 1 2) (make-rat 2 4)))

(define (rat< x y)
  "Returns #t if the rational x is lesser than the rational y"
  (rat-positive? (rat- y x)))

;; (pp (rat< (make-rat 1 2) (make-rat 2 3)))

(define (rat> x y)
  "Returns #t if the rational x y greater that the rational y"
  (rat-positive? (rat- x y)))

;; (pp (rat> (make-rat 1 2) (make-rat 2 3)))

(define (rat-max x y)
  "Returns greater of the rationals x and y"
  (if [rat> x y] x y))

;; (pp (rat-max (make-rat 1 2) (make-rat 2 3)))

(define (rat-min x y)
  "Returns lesser of the rationals x and y"
  (if [rat< x y] x y))

;; (pp (rat-min (make-rat 1 2) (make-rat 2 3)))

(define (rat-abs r)
  "Returns the absolute value of the rational r"
  (if [rat-positive? r] r (rat-opposite r)))

;; (pp (rat-abs (make-rat 1 2)))
;; (pp (rat-abs (make-rat -1 2)))
;; (pp (rat-abs (make-rat 1 -2)))
;; (pp (rat-abs (make-rat -1 -2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; POLYNOMIAL - SYMBOLIC ALGEBRA

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data abstraction primitives: constructor, zero, and selectors
(define (pcons l d p)
  "Adds a new term of degree d with lead oefficient d to the polynomial p
   (equal? p (pcons (lead p) (degree p) (tail p))) => #t"
  (define (cons-term p)
    (if [= d (1+ (degree p))] (cons l p) (cons-term (cons 0 p))))
  (let ([pd (degree p)])
    (cond
      [(zero? l) p]
      [(and (zero? d) (equal? p (pzero))) (list l)]
      [(<= d pd)
       (error
        (format #f "pcons: term degree too low: ~s (polinomial degree: ~s)" d pd))]
      [else (cons-term p)])))

;; (pp (pcons 0 1 (pzero)))
;; (pp (pcons 1 0 (pzero)))
;; (pp (pcons 1 1 (pzero)))
;; (pp (pcons 1 2 (pzero)))
;; (pp (pcons 1 4 (pcons 1 2 (pzero))))

(define (pzero)
  "Returns zero polynomial with degree zero and leading coefficient zero"
  '(0))

(define (degree p)
  "Returns the degree (exponent) of the polynomial p"
  (1- (length p)))

;; (pp (degree (pzero)))
;; (pp (degree (pcons 1 1 (pzero))))

(define (lead p)
  "Returns the leading coefficient of the polynomial p"
  (car p))

(define (tail p)
  "Returns the tail of the polynomial wihout the leading term"
  (cond
    [(zero? (degree p)) (pzero)]
    [(zero? (lead (cdr p))) (tail (cdr p))]
    [else (cdr p)]))

;; (pp (tail (pzero)))
;; (pp (tail (pcons 1 1 (pcons 1 0 (pzero)))))
;; (pp (tail (pcons 1 4 (pcons 1 2 (pzero)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data abstraction algorithms
(define (pzero? p)
  "Returns true if the leading coefficient and the degree of the polynomial p are zero"
  (and (zero? (lead p)) (zero? (degree p))))

(define (p+ x y)
  "Adds two polinomials x and y"
  (let add* ([x x] [y y] [r '()])
    (cond
      [(and (pzero? x) (pzero? y))
       (fold (lambda (t b) (pcons (car t) (cdr t) b)) (pzero) r)]
      [(> (degree x) (degree y))
       (add* (tail x) y (cons (cons (lead x) (degree x)) r))]
      [(> (degree y) (degree x))
       (add* x (tail y) (cons (cons (lead y) (degree y)) r))]
      [else
       (add* (tail x) (tail y) (cons (cons (+ (lead x) (lead y)) (degree x)) r))])))

(let* ([p1 (pcons 1 0 (pzero))]
       [p2 (pcons 2 1 p1)]
       [p3 (pcons 3 2 p2)]
       [p4 (pcons 4 3 p3)]
       [p5 (pcons 5 4 (pzero))])
  (pp (p+ p1 p1))
  (pp (p+ p1 p2))
  (pp (p+ p2 p3))
  (pp (p+ p3 p4))
  (pp (p+ p5 p1))
  )
