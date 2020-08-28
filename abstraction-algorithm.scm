(define-module
  (abstraction-algorithm)
  #:export
  ;; FRACTION - EXACT ARITHMETIC
  (make-rat numer denom rat-zero? rat-positive? rat-negative? rat+ rat-opposite
            rat- rat* rat-inverse rat/ rat= rat< rat> rat-min mat-max rat-abs
            ;; POLYNOMIAL - SYMBOLIC ALGEBRA
            pcons pzero degree lead tail pzero? p+ p* poly-opposite p- poly-quot+rem
            poly-value digits->poly number->decimal decimal->number))

(use-modules
 (ice-9 receive)
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
        (format #f "pcons: term degree too low: ~s (polynomial degree: ~s)" d pd))]
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
  (if [zero? (degree p)] (pzero) (cdr p)))

;; (pp (tail (pzero)))
;; (pp (tail (pcons 1 1 (pcons 1 0 (pzero)))))
;; (pp (tail (pcons 1 4 (pcons 1 2 (pzero)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data abstraction algorithms

(define (leads->poly l)
  "Constructs a polynomial from reverse list of pairs ((lead . degree) ...)"
  (fold (lambda (t b) (pcons (car t) (cdr t) b)) (pzero) l))

(define (pzero? p)
  "Returns true if the leading coefficient and the degree of the polynomial p are zero"
  (and (zero? (lead p)) (zero? (degree p))))

(define (p+ x y)
  "Adds two polynomials x and y"
  (let add* ([x x] [y y] [r '()])
    (cond
      [(and (pzero? x) (pzero? y)) (leads->poly r)]
      [(> (degree x) (degree y))
       (add* (tail x) y (cons (cons (lead x) (degree x)) r))]
      [(> (degree y) (degree x))
       (add* x (tail y) (cons (cons (lead y) (degree y)) r))]
      [else
       (add* (tail x) (tail y) (cons (cons (+ (lead x) (lead y)) (degree x)) r))])))

(define (p* x y)
  "Multiplicates two polynomials x and y"
  (define (mul-term t p)
    (let mul-term* ([p p] [r '()])
      (if [pzero? p] (leads->poly r)
          (mul-term* (tail p)
                     (cons (cons (* (lead t) (lead p)) (+ (degree t) (degree p))) r)))))
  (let mul* ([x x] [r (pzero)])
    (if [pzero? x] r
        (mul* (tail x) (p+ (mul-term x y) r)))))

(define (poly-opposite p)
  "Returns a polynomial that is opposite to the polynomial p"
  (p* (pcons -1 0 (pzero)) p))

(define (p- x y)
  "Subtracts the polynomial y from the polynomial x"
  (p+ x (poly-opposite y)))

(define (poly-quot+rem x y)
  "Returns the quotient and remainder of devision of the polynomials x by y"
  (when [pzero? y] (error "poly-quot+rem: division by zero polynomial"))
  (let quot* ([x x] [q '()])
    (if [or (< (degree x) (degree y)) (pzero? x)]
        (values (leads->poly q) x)
        (let* ([l (/ (lead x) (lead y))]
               [d (- (degree x) (degree y))]
               [pd (pcons l d (pzero))]
               [sb (p* pd y)]
               [df (p- x sb)])
          (quot* df (cons (cons l d) q))))))

(define (poly-value p v)
  "Evaluates the polynomial p for the value v"
  (let value* ([p p] [r 0])
    ;; Method of nested multiplication for polynomial evaluation
    (if [zero? (degree p)] (+ r (lead p))
        (value* (tail p) (* (+ r (lead p)) v)))))

#;(let* ([p1 (pcons 1 0 (pzero))]
       [p2 (pcons 2 1 p1)]
       [p3 (pcons 3 2 p2)]
       [p4 (pcons 4 3 p3)]
       [p5 (pcons 5 4 (pzero))]
       [p6 (pcons 1 2 (pcons 2 0 (pzero)))]
       [p7 (pcons 2 3 (pcons 1 1 (pzero)))])
  ;; (pp (p+ p1 p1))
  ;; (pp (p+ p1 p2))
  ;; (pp (p+ p2 p3))
  ;; (pp (p+ p3 p4))
  ;; (pp (p+ p5 p1))
  ;; (pp (p+ p6 p7))
  ;; (pp (p* p1 p1))
  ;; (pp (p* p1 p2))
  ;; (pp (p* p2 p2))
  ;; (pp (p* p2 p3))
  ;; (pp (p* p2 p5))
  ;; (pp (p* p6 p7))
  ;; (pp (poly-opposite p4))
  ;; (pp (p- p1 p1))
  ;; (pp (p- p1 p2))
  ;; (pp (p- p1 p2))
  ;; (pp (p- p3 p2))
  ;; (pp (p- p5 p1))
  ;; (pp (poly-value p1 1))
  ;; (pp (poly-value p2 2))
  ;; (pp (poly-value p3 3))
  ;; (pp (poly-value p4 2))
  ;; (pp (poly-value p5 1))
  ;; (pp (poly-value p6 3))
  ;; (pp (poly-value p7 3))
  #;(receive (q r) (poly-quot+rem p1 (pzero))
      (format #t "~s ~s\n" q r))
  ;; (receive (q r) (poly-quot+rem (pzero) p1)
  ;;   (format #t "~s ~s\n" q r))
  ;; (receive (q r) (poly-quot+rem p1 p1)
  ;;   (format #t "~s ~s\n" q r))
  ;; (receive (q r) (poly-quot+rem p3 p3)
  ;;   (format #t "~s ~s\n" q r))
  ;; (receive (q r) (poly-quot+rem p2 p1)
  ;;   (format #t "~s ~s\n" q r))
  ;; (receive (q r) (poly-quot+rem p3 p1)
  ;;   (format #t "~s ~s\n" q r))
  ;; (receive (q r) (poly-quot+rem p3 p2)
  ;;   (format #t "~s ~s\n" q r))
  ;; (receive (q r) (poly-quot+rem p4 p2)
  ;;   (format #t "~s ~s\n" q r))
  ;; (receive (q r) (poly-quot+rem p5 p1)
  ;;   (format #t "~s ~s\n" q r))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NUMBER CONVERSION

(define (digits->poly ds)
  "Converts the list of digits ds into a polynomial"
  (let convert* ([ds ds] [d (1- (length ds))] [r '()])
    (if [null? ds] (leads->poly r)
        (convert* (cdr ds) (1- d) (cons (cons (car ds) d) r)))))

;; (pp (digits->poly '(1 0 1 0)))

(define* (number->decimal ds #:optional (radix 10))
  "Converts the list of digits ds in the radix into decimal"
  (let ([p (digits->poly ds)])
    (poly-value p radix)))

;; (pp (number->decimal '(1 0 1 0) 2))
;; (pp (number->decimal '(1 1 1 1) 2))
;; (pp (number->decimal '(0 1 1 1) 2))
;; (pp (number->decimal '(0 1 0 1) 2))
;; (pp (number->decimal '(0 1 0 1)))

(define* (decimal->number x #:optional (radix 10))
  "Converts the decimal number x into a list of digits in the radix"
  (unfold-right
   zero?
   (lambda (s) (remainder s radix))
   (lambda (s) (quotient s radix))
   x))

;; (pp (decimal->number 10 2))
;; (pp (decimal->number 8 2))
;; (pp (decimal->number 7 2))
;; (pp (decimal->number 1234))
;; (pp (number->decimal (decimal->number 1234 2) 2))
;; (pp (number->decimal (decimal->number 1234 5) 5))
