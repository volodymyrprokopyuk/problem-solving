(define-module
  (abstraction-algorithm)
  #:export ())

(use-modules
 ((ice-9 pretty-print)
  #:select ((pretty-print . pp))))

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
