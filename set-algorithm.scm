(define-module
  (set-algorithm)
  #:export (both at-least-one neither empty-set empty-set? pick adjoin exclude))

(use-modules
 (ice-9 curried-definitions)
 ((ice-9 pretty-print)
  #:select ((pretty-print . pp))))

(define ((both p) x y)
  "Returns #t when both x and y satisfy the predictae p"
  (and (p x) (p y)))

(define (both2 p)
  "Implements both in therms of at-least-one"
  (negate (at-least-one (negate p))))

(define (both3 p)
  "Implements both in therms of neither"
  (neither (negate p)))

;; (pp ((both null?) '() '()))
;; (pp ((both2 null?) '() '()))
;; (pp ((both3 null?) '() '()))

(define ((at-least-one p) x y)
  "Returns #t if at least one x or y satisfies the predicate p"
  (or (p x) (p y)))

(define (at-least-one2 p)
  "Implements at-least-one in terms of both"
  (negate (both (negate p))))

(define (at-least-one3 p)
  "Implements at-least-one in terms of neither"
  (negate (neither p)))

;; (pp ((at-least-one null?) '(a) '()))
;; (pp ((at-least-one2 null?) '(a) '()))
;; (pp ((at-least-one3 null?) '(a) '()))

(define ((neither p) x y)
  "Returns #t when neither x nor y satisfies the predicate p"
  (not (or (p x) (p y))))

(define (neither2 p)
  "Implements neither in terms of both"
  (both (negate p)))

(define (neither3 p)
  "Implements neither in terms of at-least-one"
  (negate (at-least-one p)))

;; (pp ((neither null?) '(a) '(b)))
;; (pp ((neither2 null?) '(a) '(b)))
;; (pp ((neither3 null?) '(a) '(b)))

(define (equal2? x y)
  "Recursively tests components for equality"
  (cond
    [((neither pair?) x y) (eqv? x y)]
    [((both pair?) x y) (and (equal2? (car x) (car y)) (equal2? (cdr x) (cdr y)))]
    [else #f]))

;; (pp (equal2? 'a 'a))
;; (pp (equal2? 'a 'b))
;; (pp (equal2? '(a) '(a)))
;; (pp (equal2? '(a) '(b)))
;; (pp (equal2? '(a b (c (d (e f) g) h i)) '(a b (c (d (e f) g) h i))))
;; (pp (equal2? '(a b (c (d (e F) g) h i)) '(a b (c (d (e f) g) h i))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SET ALGEBRA

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data abstraction primitives

(define (empty-set)
  "Returns the empty set"
  '())

(define (empty-set? s)
  "Returns #t if the set s is empty"
  (eq? s (empty-set)))

(define (pick s)
  "Returns randomly selected element from the set s"
  (when (empty-set? s) (error "pick: empty set"))
  (car s))

(define (adjoin e s)
  "Adjoins the element e to the set s if the element is not already in the set"
  (let adjoin* ([ss s])
    (cond
      [(empty-set? ss) (cons e s)]
      [(equal? e (car ss)) s]
      [else (adjoin* (cdr ss))])))

;; (pp (adjoin 'a (empty-set)))
;; (pp (adjoin 'b (adjoin 'a (empty-set))))

(define ((exclude e) s)
  "Removes the element e from the set s if the element is in the set"
  (let exclude* ([ss s] [r (empty-set)])
    (cond
      [(empty-set? ss) s]
      [(equal? e (car ss)) (append r (cdr ss))]
      [else (exclude* (cdr ss) (cons (car ss) r))])))

;; (pp ((exclude 'A) '(a b c)))
;; (pp ((exclude 'b) '(a b c)))
;; (pp ((exclude 'c) '(a b c d e)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data abstraction algorithms
