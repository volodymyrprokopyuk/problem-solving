(use math.const)

;; *** Chapter 1 - Computer science and programming

(define (total-price p :optional (t 0.05)) (+ p (* p t)))

;; #?=(total-price 10)
;; #?=(total-price 10 0)

(define (cylinder-volume r h) (* h pi r r))

;; #?=(cylinder-volume 5 4)

(define (income-tax i :optional (m 1e4) (t 0.2))
  (cond [(< i m) 0] [else (* (- i m) t)]))

;; #?=(income-tax 9000)
;; #?=(income-tax 30000)
;; #?=(income-tax 12500)

(define (average . a) (/ (apply + a) (length a)))

;; #?=(average 1 2 3 4)

;; *** Chapter 2 - Recursion and induction

(define (factorial n)
  (let factorial* ([n n] [r 1])
    (cond [(< n 2) r] [else (factorial*(- n 1) (* r n))])))

;; #?=(factorial 0)
;; #?=(factorial 4)
;; #?=(factorial 52)

(define (power b k)
  (let power* ([k k] [r 1])
    (cond [(zero? k) r] [else (power* (- k 1) (* r b))])))

;; #?=(power 2 0)
;; #?=(power 2 3)

;; Mathematical induction
;; 1. Base case is correct by definition x = 0
;; 2. Induction hypothesis assumes correctness of x - 1
;; 3. Inductive step proves correctness of x
(define (square x)
  (cond [(zero? x) 0] [else (+ (square (- x 1)) (* 2 x) -1)]))

;; #?=(square 2)
;; #?=(square 3)

(define (=quotient x y)
  (let quotient* ([x x] [r 0])
    (cond
      [(< x 0) (- (=quotient (- x) y))]
      [(< y 0) (- (=quotient x (- y)))]
      [(< x y) r]
      [else (quotient* (- x y) (+ r 1))])))

;; #?=(=quotient 3 4)
;; #?=(=quotient -4 4)
;; #?=(=quotient 5 -4)
;; #?=(=quotient -9 -4)

(define (mul x y)
  (let mul* ([y y] [r 0])
    (cond
      [(< x 0) (- (mul (- x) y))]
      [(< y 0) (- (mul x (- y)))]
      [(zero? y) r]
      [else (mul* (- y 1) (+ r x))])))

;; #?=(mul 2 0)
;; #?=(mul -2 1)
;; #?=(mul 2 -2)
;; #?=(mul -2 -3)

(define (number-of-digits n :optional (b 10))
  (let nod* ([n n] [r 1])
    (cond
      [(negative? n) (number-of-digits (- n))]
      [(< n b) r]
      [else (nod* (quotient n b) (+ r 1))])))

;; #?=(number-of-digits 0)
;; #?=(number-of-digits 12)
;; #?=(number-of-digits -345)

(define (integer-factorization x)
  (let fact* ([x x] [n 0])
    (cond [(odd? x) (values n x)] [else (fact* (quotient x 2) (+ n 1))])))

(for-each
 (lambda (x)
   (receive (n k) (integer-factorization x) (format #t "~a = 2^~a ~a\n" x n k)))
 '(1 2 3 4 5 6 7 8 9 10))
