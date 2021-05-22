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
