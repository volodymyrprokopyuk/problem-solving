(define-module
  (set-algorithm)
  #:export (both at-least-one neither set-empty set-empty? pick adjoin exclude))

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

(define (set-empty)
  "Returns the empty set"
  '())

(define (set-empty? s)
  "Returns #t if the set s is empty"
  (eq? s (set-empty)))

(define (pick s)
  "Returns randomly selected element from the set s"
  (when (set-empty? s) (error "pick: empty set"))
  (car s))

(define (adjoin e s)
  "Adjoins the element e to the set s if the element is not already in the set"
  (let adjoin* ([ss s])
    (cond
      [(set-empty? ss) (cons e s)]
      [(equal? e (car ss)) s]
      [else (adjoin* (cdr ss))])))

;; (pp (adjoin 'a (set-empty)))
;; (pp (adjoin 'b (adjoin 'a (set-empty))))

(define ((exclude e) s)
  "Removes the element e from the set s if the element is in the set"
  (let exclude* ([ss s] [r (set-empty)])
    (cond
      [(set-empty? ss) s]
      [(equal? e (car ss)) (append r (cdr ss))]
      [else (exclude* (cdr ss) (cons (car ss) r))])))

;; (pp ((exclude 'A) '(a b c)))
;; (pp ((exclude 'b) '(a b c)))
;; (pp ((exclude 'c) '(a b c d e)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data abstraction algorithms

(define (make-set . e)
  "Creates a set of the elements es"
  (let make* ([e e] [r (set-empty)])
    (if [null? e] r (make* (cdr e) (adjoin (car e) r)))))

;; (pp (make-set 'a 'b 'c))

(define (cardinal s)
  "Returns the number of elements in the set s"
  (let cardinal* ([s s] [r 0])
    (if [set-empty? s] r
        (let ([e (pick s)]) (cardinal* ((exclude e) s) (1+ r))))))

;; (pp (cardinal (make-set 1 2 3 4)))

(define ((set-every p) s)
  "Returns #t if every element in the set s satisfies the predicate p"
  (let every* ([s s])
    (if [set-empty? s] #t
        (let ([e (pick s)])
          (if [p e] (every* ((exclude e) s)) #f)))))

(define (set-every2 p)
  "Implements set-every in terms of set-any"
  (negate (set-any (negate p))))

(define set-every21 (compose negate set-any negate))

(define (set-every3 p)
  "Implements set-every in terms of set-none"
  (set-none (negate p)))

;; (pp ((set-every even?) (make-set 2 4 6 8)))
;; (pp ((set-every2 even?) (make-set 2 4 6 8)))
;; (pp ((set-every3 even?) (make-set 2 4 6 8)))
;; (pp ((set-every even?) (make-set 2 4 6 8 9)))
;; (pp ((set-every2 even?) (make-set 2 4 6 8 9)))
;; (pp ((set-every3 even?) (make-set 2 4 6 8 9)))

(define ((set-any p) s)
  "Returns #t if at least one (any) element in the set s satisfies the predicate p"
  (let any* ([s s])
    (if [set-empty? s] #f
        (let ([e (pick s)])
          (if [p e] #t (any* ((exclude e) s)))))))

(define (set-any2 p)
  "Implements set-any in termos of set-every"
  (negate (set-every (negate p))))

(define (set-any3 p)
  "Implements set-any in termos of set-none"
  (negate (set-none p)))

;; (pp ((set-any even?) (make-set 1 3 5 7)))
;; (pp ((set-any2 even?) (make-set 1 3 5 7)))
;; (pp ((set-any3 even?) (make-set 1 3 5 7)))
;; (pp ((set-any even?) (make-set 1 3 5 7 8)))
;; (pp ((set-any2 even?) (make-set 1 3 5 7 8)))
;; (pp ((set-any3 even?) (make-set 1 3 5 7 8)))

(define ((set-none p) s)
  "Returns #t if none of elemnts in the set s satisfies the predicate p"
  (let none* ([s s])
    (if [set-empty? s] #t
        (let ([e (pick s)])
          (if [p e] #f (none* ((exclude e) s)))))))

(define (set-none2 p)
  "Implements set-none in terms of set-every"
  (set-every (negate p)))

(define (set-none3 p)
  "Implements set-none in terms of set-any"
  (negate (set-any p)))

;; (pp ((set-none even?) (make-set 1 3 5 7)))
;; (pp ((set-none2 even?) (make-set 1 3 5 7)))
;; (pp ((set-none3 even?) (make-set 1 3 5 7)))
;; (pp ((set-none even?) (make-set 1 3 5 7 8)))
;; (pp ((set-none2 even?) (make-set 1 3 5 7 8)))
;; (pp ((set-none3 even?) (make-set 1 3 5 7 8)))

(define ((set-member? e) s)
  "Returns #t if the element e is in the set s"
  ((set-any (lambda (se) (equal? se e))) s))

;; (pp ((set-member? 1) (make-set 1 2 3 4)))

(define ((set-subset? x) y)
  "Returns #t if the set x is a subset of the set y"
  ((set-every (lambda (xe) ((set-member? xe) y))) x))

;; (pp ((set-subset? (make-set 1 2 3 4)) (make-set 1 2 3)))

(define ((set-equal? x) y)
  "Returns #t if the sets x and y are equal"
  (and ((set-subset? x) y) ((set-subset? y) x)))

;; (pp ((set-equal? (make-set 1 2 3 4)) (make-set 1 2 3 4)))

(define set-intersection
  (case-lambda
    [(x y)
     "Returns the intersaction of the sets x and y"
     (let intersect* ([x x] [r (set-empty)])
       (if [set-empty? x] r
           (let ([e (pick x)])
             (if [(set-member? e) y]
                 (intersect* ((exclude e) x) (adjoin e r))
                 (intersect* ((exclude e) x) r)))))]
    [(x y . r)
     "Returns the intersaction of more than two sets"
     (apply set-intersection (set-intersection x y) r)]))

;; (pp (set-intersection
;;      (make-set 'a 'b 'c 'd) (make-set 'a 'c 'e 'f) (make-set 'a 'g 'h)))

(define set-union
  (case-lambda
    [(x y)
     "Returns the union of the sets x and y"
     (let union* ([x x] [r y])
       (if [set-empty? x] r
           (let ([e (pick x)])
             (if [(set-member? e) y]
                 (union* ((exclude e) x) r)
                 (union* ((exclude e) x) (adjoin e r))))))]
    [(x y . r)
     "Returns the union of more than two sets"
     (apply set-union (set-union x y) r)]))

;; (pp (set-union
;;      (make-set 'a 'b 'c 'd) (make-set 'a 'c 'e 'f) (make-set 'a 'g 'h)))

(define set-difference
  (case-lambda
    [(x y)
     "Returns the difference of the sets x and y"
     (let diff* ([x x] [r x])
       (if [set-empty? x] r
           (let ([e (pick x)])
             (if [(set-member? e) y]
                 (diff* ((exclude e) x) ((exclude e) r))
                 (diff* ((exclude e) x) r)))))
     ]
    [(x y . r)
     "Returns the difference of more than two sets"
     (apply set-difference (set-difference x y) r)]))

;; (pp (set-difference
;;      (make-set 'a 'b 'c 'd) (make-set 'a 'c 'e 'f) (make-set 'd 'g 'h)))

(define (set-symmetric-difference x y)
  "Returns the symmetric difference of the sets x and y"
  (set-difference (set-union x y) (set-intersection x y)))

(define (set-symmetric-difference2 x y)
  "Returns the symmetric difference of the sets x and y"
  (set-union (set-difference x y) (set-difference y x)))

;; (pp (set-symmetric-difference (make-set 'a 'b 'c 'd) (make-set 'a 'c 'e 'f)))
;; (pp (set-symmetric-difference2 (make-set 'a 'b 'c 'd) (make-set 'a 'c 'e 'f)))

(define (set-map f s)
  "Returns a new set by applying the procedure f to every element of the set s"
  (let map* ([s s] [r (set-empty)])
    (if [set-empty? s] r
        (let ([e (pick s)])
          (map* ((exclude e) s) (adjoin (f e) r))))))

;; (pp (set-map 1+ (make-set 1 2 3 4)))
