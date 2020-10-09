(define-module
  (laze-algorithm))

(use-modules
 (srfi srfi-1)
 (srfi srfi-27)
 ((recursive-algorithm)
  #:select (factorial fibonacci prime?))
 ((ice-9 pretty-print)
  #:select ((pretty-print . pp))))

(random-source-randomize! default-random-source)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DALAYED (LAZY) LIST

(define dl-null '())

(define dl-null? null?)

(define dl-car car)

(define dl-cdr (compose force cdr))

(define-syntax-rule (dl-cons e dl) (cons e (delay dl)))

(define (make-dlist . args)
  "Creates finite delated list from the arguments args"
  (let dlist* ([l (reverse args)] [r dl-null])
    (cond
      [(null? l) r]
      [else (dlist* (cdr l) (dl-cons (car l) r))])))

(define (dlist->list dl)
  "Forces the finite delayed list dl into a list"
  (let list* ([dl dl] [r '()])
    (cond
      [(dl-null? dl) (reverse r)]
      [else (list* (dl-cdr dl) (cons (dl-car dl) r))])))

;; (let ([dl (make-dlist 1 2 3 4 5)])
;;   (pp (dlist->list dl)))

(define (dl-unfold p f g s)
  "Fundamental constructor of a finite/infinite delayed list. Not tail-recursive"
  (let ([s (g s)])
    (cond
      [(p s) dl-null]
      [else (dl-cons (f s) (dl-unfold p f g s))])))

;; (let ([dl (dl-unfold (lambda (e) (> e 5)) identity 1+ 0)]
;;       [rdl (dl-unfold (lambda (i) (> i 10)) (lambda (_) (random-integer 10)) 1+ 0)])
;;   (pp (dlist->list dl))
;;   (pp (dlist->list rdl)))

(define (dl-fold f b dl)
  "Fundamental iterator for a finite delayed list dl
   fold: (f b e) tail-recursive. b carries intermediate/final result"
  (cond
    [(dl-null? dl) b]
    [else (dl-fold f (f b (dl-car dl)) (dl-cdr dl))]))

;; (let ([s (dl-fold + 0 (make-dlist 1 2 3))]
;;       [d (dl-fold - 0 (make-dlist 1 2 3))]
;;       [dl (dl-fold (lambda (b e) (dl-cons e b)) dl-null (make-dlist 1 2 3 4 5))])
;;   (pp s) (pp d) (pp (dlist->list dl)))

(define (dl-fold-right f b dl)
  "Fundamental iterator for the finite delayed list dl
   fold-right: (f e b) not tail-recursive. b is forwarded to the end"
  (cond
    [(dl-null? dl) b]
    [else (f (dl-car dl) (dl-fold-right f b (dl-cdr dl)))]))

;; (let ([s (dl-fold-right + 0 (make-dlist 1 2 3))]
;;       [d (dl-fold-right - 0 (make-dlist 1 2 3))]
;;       [dl (dl-fold-right (lambda (e b) (dl-cons e b)) dl-null (make-dlist 1 2 3 4 5))])
;;   (pp s) (pp d) (pp (dlist->list dl)))

(define (dl-reverse dl)
  "Reverses the finite delayed list dl"
  (dl-fold (lambda (b e) (dl-cons e b)) dl-null dl))

;; (let ([dl (make-dlist 1 2 3 4 5)])
;;   (pp (dlist->list (dl-reverse dl))))

#;(define (dl-map f dl)
  "Maps the unary function f over the finite/infinite delayed list dl"
  (cond
    [(dl-null? dl) dl-null]
    [else (dl-cons (f (dl-car dl)) (dl-map f (dl-cdr dl)))])
  #;(dl-reverse (dl-fold (lambda (b e) (dl-cons (f e) b)) dl-null dl)))

(define (dl-map f . dl)
  "Maps the n-ary function f over the finite/infinite delayed list dl"
  (cond
    [(any dl-null? dl) dl-null]
    [else (dl-cons (apply f (map dl-car dl)) (apply dl-map f (map dl-cdr dl)))]))

;; (let ([dl (dl-map 1+ (make-dlist 1 2 3 4 5))]
;;       [dl2 (dl-map + (make-dlist 1 2 3 4 5 6) (make-dlist 10 20 30 40 50))])
;;   (pp (dlist->list dl)) (pp (dlist->list dl2)))

(define (dl-filter p dl)
  "Filters the finite/infinite delayed list dl with the predicate p"
  (cond
    [(dl-null? dl) dl-null]
    [(p (dl-car dl)) (dl-cons (dl-car dl) (dl-filter p (dl-cdr dl)))]
    [else (dl-filter p (dl-cdr dl))])
  #;(dl-reverse (dl-fold (lambda (b e) (if [p e] (dl-cons e b) b)) dl-null dl)))

;; (let ([dl (make-dlist 1 2 3 4 5)])
;;   (pp (dlist->list (dl-filter odd? dl))))

(define (dl-append x y)
  "Appends the finite delayed list y to the finite delayed list x"
  (dl-fold (lambda (b e) (dl-cons e b)) y (dl-reverse x)))

;; (let ([dl (dl-append (make-dlist 1 2 3) (make-dlist 4 5 6))])
;;   (pp (dlist->list dl)))

(define (dl-take n dl)
  "Returns n first elements from the finite/infinite delayed list dl"
  (let take* ([dl dl] [n n] [r '()])
    (cond
      [(or (zero? n) (dl-null? dl)) (reverse r)]
      [else (take* (dl-cdr dl) (1- n) (cons (dl-car dl) r))])))

(define* (dl-integer #:optional (start 0) (step 1))
  "Returns an infinite stream (delayed list) of integers"
  (dl-unfold (const #f) identity (lambda (s) (+ s step)) start))

;; (pp (dl-take 7 (make-dlist 1 2 3)))
;; (pp (dl-take 7 (dl-integer)))
;; (pp (dl-take 7 (dl-integer 0 10)))
;; (pp (dl-take 7 (dl-map 1+ (dl-integer))))
;; (pp (dl-take 7 (dl-map (lambda (e) (expt 2 e)) (dl-integer))))
;; (pp (dl-take 7 (dl-filter odd? (dl-integer))))

(define (dl-random)
  "Returns an infinite stream (delayed list) of random integers"
  (dl-unfold (const #f) (lambda (_) (random-integer 10)) identity 0))

;; (pp (dl-take 7 (dl-random)))

(define (dl-factorial)
  "Returns an infitine stream (delaye list) of factorials"
  (dl-unfold (const #f) factorial 1+ 0))

;; (pp (dl-take 7 (dl-factorial)))

(define (dl-fibonacci)
  "Returns an infinite stream (delayed list) of fibonacci sequence"
  (dl-unfold (const #f) fibonacci 1+ -1))

;; (pp (dl-take 15 (dl-fibonacci)))

(define (dl-prime)
  "Returns an infinite stream (delayed list) of prime numbers"
  (dl-filter prime? (dl-integer)))

;; (pp (dl-take 20 (dl-prime)))
