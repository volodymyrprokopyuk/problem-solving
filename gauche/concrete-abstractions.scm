(use math.const)
(use gauche.generator)

;; *** Chapter 1 - Computer science and programming

;; A procedure is an algorithm implementation and performs a computational process

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

;; Recursion = solve a subproblem first, then do the little bit of work that is left to
;; solve the main problem (not tail-recursive). Put the main problem on hold while a
;; subproblem is being solved. Less efficient due to the main problem memory overhead (the
;; deepper is the recusion the more memory is consumed)

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

;; (for-each
;;  (lambda (x)
;;    (receive (n k) (integer-factorization x) (format #t "~a = 2^~a ~a\n" x n k)))
;;  '(1 2 3 4 5 6 7 8 9 10))

;; *** Chapter 3 - Iteration and invariants

;; Iteration = transform the main problem into a subproblem with the same solution by
;; doing a little bit of work first (tail-recursive named let). Progressively reduce the
;; main problem by solfving the first subproblem first. More efficient as the only ever
;; single subproblem is being solved in a fixed amount of memory no matter how many
;; iterations are there

;; Invariant = some parameters keep moving towards the base case, while some other
;; parameters change in a compansatory fashion to keep the invariant fixed. Invariants
;; are used to define iterative procedures and to prove their correctness (verified base
;; case -> assume induction hypothesis -> prove inductive step)

(define (fermat-number n)
  (let fermat* ([n n] [r 2])
    (cond [(zero? n) (+ r 1)] [else (fermat* (- n 1) (expt r 2))])))

;; (for-each (lambda (n) (format #t "~a " (fermat-number n))) '(0 1 2 3 4 5 6))

(define (perfect-number? x)
  ($ = x $ fold + 0 $ filter (.$ zero? (cut remainder x <>)) $ iota (- x 1) 1))

(define (perfect-number2? x)
  (let perfect* ([i (- x 1)] [s 0])
    (cond
      [(zero? i) (= s x)]
      [(zero? (remainder x i)) (perfect* (- i 1) (+ s i))]
      [else (perfect* (- i 1) s)])))

(define (perfect-number3? x)
  (let* ([i (iota (- (floor (sqrt x)) 1) 2)]
         [l (filter (.$ zero? (cut remainder x <>)) i)]
         [h (map (cut / x <>) l)]
         [d (if [null? l] '() (append '(1) l h))]
         [s (fold + 0 d)])
    (= s x)))

(define (perfect-numbers)
  ($ gfilter perfect-number3? $ grange 1))

;; #?=(generator->list (gtake (perfect-numbers) 4)) ;; Perfect numbers 6 28 496 8128

(define (golden-ratio :optional (p 1e-1))
  (let golden* ([r 1])
    (let ([r2 (+ 1 (/ r))])
      (if [< (abs (- r2 r)) p] r2 (golden* r2)))))

;; (for-each (lambda (p) (print (golden-ratio p))) '(1e-1 1e-2 1e-3 1e-4 1e-5 1/50000))

(define (every-n-of? p l n)
  (let every* ([p p] [l l])
    (cond
      [(< l n) #f]
      [(= p n) #t]
      [else (every* (if [< p n] (+ l (- p n)) (- p n)) (- l 1))])))

;; #?=(every-n-of? 2 8 3)
;; #?=(every-n-of? 5 8 3)
;; #?=(every-n-of? 4 8 3)
;; #?=(every-n-of? 7 8 3)
;; #?=(remove (cut every-n-of? <> 5 4) (iota 5 1))
;; #?=(remove (cut every-n-of? <> 40 3) (iota 40 1))

(define (falling-factorial x n)
  (let falling* ([n n] [r 1])
    (cond [(zero? n) r] [else (falling* (- n 1) (* r (- x n -1)))])))

;; #?=(falling-factorial 0 0)
;; #?=(falling-factorial 5 1)
;; #?=(falling-factorial 5 4)
;; #?=(falling-factorial 7 3)

(define (power2 x n)
  (let power* ([n n] [r 1])
    (cond
      [(zero? n) r]
      [(positive? n) (power* (- n 1) (* r x))]
      [else (power* (+ n 1) (/ r x))])))

;; #?=(power2 2 0)
;; #?=(power2 2 3)
;; #?=(power2 2 -3)

;; Chapter 4 - Order of growth and tree recursion

;; Asymptotic approximation of which algorithm is more rapidly taking longer on bigger
;; input sizes

;; Tree recursion = solve main problem by solving several subproblems first than combine
;; the results to get the solution to the main problem

;; Digital signature = (digest message) -> digest # one-way, many-to-one function
;;   - (sign digest private-key) -> signature # inverse of (verify)
;;   - (verify message signature public-key) -> valid / invalid # inverse of (sign)
;; s = x^e mod m # (sign) # e = private-key, m public-key
;; x = s^3 mode m # (verify)
;; xy mod m = (x mod m) * (y mod m) mod m

(define (merge-sort l :optional (c <))
  (let* ([n (length l)] [m (quotient n 2)])
    (cond
      [(< n 2) l]
      [else
       (let sort* ([a (merge-sort (take l m) c)] [b (merge-sort (drop l m) c)] [r '()])
         (cond
           [(null? a) (reverse (append (reverse b) r))]
           [(null? b) (reverse (append (reverse a) r))]
           [(c (car a) (car b)) (sort* (cdr a) b (cons (car a) r))]
           [else (sort* a (cdr b) (cons (car b) r))]))])))

#?=(merge-sort '(4 8 3 1 5 1 2 0 9 7 6 9))
#?=(merge-sort '(4 8 3 1 5 1 2 0 9 7 6 9) >)
