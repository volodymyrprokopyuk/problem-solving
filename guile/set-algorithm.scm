(define-module
  (set-algorithm)
  #:export (both at-least-one neither set-empty set-empty? pick adjoin exclude
                 make-set cardinal set-every set-any set-none set-member? set-subset?
                 set-equal? set-intersection set-union set-difference
                 set-symmetric-difference set-map set-filter set-fold set-unfold
                 power-set))

(use-modules
 (ice-9 curried-definitions)
 (ice-9 match)
 (srfi srfi-1) ;; List library
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
;; (pp ((set-every21 even?) (make-set 2 4 6 8)))
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

(define (set-filter p s)
  "Returns a new set of every element from the set s that satisfies the predicate p"
  (let filter* ([s s] [r (set-empty)])
    (if [set-empty? s] r
        (let ([e (pick s)])
          (if [p e] (filter* ((exclude e) s) (adjoin e r))
              (filter* ((exclude e) s) r))))))

;; (pp (set-filter even? (make-set 1 2 3 4 5 6)))

(define (set-fold f b s)
  "Folds the function f over the set s starting from the base b (fundamental iterator)"
  (let fold* ([s s] [r b])
    (cond
      [(set-empty? s) r]
      [else
       (let* ([e (pick s)])
         (fold* ((exclude e) s) (f e r)))])))

;; (pp (set-fold + 0 (make-set 1 2 3 4 5 6)))
;; (pp (set-fold string-append "" (make-set "V" "l" "a" "d")))

(define (set-unfold p f g s)
  "Unfolds the seed s with g by applying f until p (fundamental constructor)"
  (let unfold* ([s s] [r (set-empty)])
    (cond
      [(p s) r]
      [else (unfold* (g s) (adjoin (f s) r))])))

;; (pp (set-unfold (lambda (s) (> s 9)) 1+ 1+ 0))
;; (pp (set-unfold string-null? (lambda (s) (string-take s 1))
;;                 (lambda (s) (string-drop s 1)) "Vlad"))

(define (power-set s)
  "Returns the power set of the set s"
  (if [set-empty? s] (make-set (set-empty))
      (let* ([e (pick s)]
             [ps (power-set ((exclude e) s))])
        (set-union ps (set-map (lambda (pse) (adjoin e pse)) ps)))))

;; (pp (power-set (make-set 'a 'b 'c)))

;; employee: name (UQ) id (UQ) age year manager salary
(define emp
  (apply make-set '(("Smith" 2324 43 1974 "Fox" 49325)
                    ("Jones" 1888 54 1965 "" 65230)
                    ("White" 3403 34 1982 "Smith" 27300)
                    ("Williams" 2451 46 1970 "Jason" 41050)
                    ("Brown" 3620 28 1984 "Williams" 18500)
                    ("August" 2221 45 1971 "Jason" 44100)
                    ("Jason" 1990 55 1965 "Jones" 63700)
                    ("Wilson" 2455 46 1970 "August" 41050)
                    ("Black" 3195 38 1978 "Smith" 31420)
                    ("Fox" 2400 41 1981 "Jason" 52200)
                    ("Blue" 3630 26 1984 "Williams" 18500))))

;; Are all employees over 25 years?
;; (pp ((set-every (match-lambda [(name id age year manager salary) (> age 25)])) emp))

;; Are there anyone under 50 years of age and over 50 000 or salary?
;; (pp ((set-any
;;       (match-lambda
;;         [(name id age year manager salary) (and (< age 50) (> salary 50000))])) emp))

;; Relation projection
;; (pp (set-map
;;      (match-lambda [(name id age year manager salary) (list name age salary)]) emp))

;; Employees over 45 years of age
;; (pp (set-filter (match-lambda [(name id age year manager salary) (> age 45)]) emp))

;; Closest common manager of existing employees
(define (employee-closest-common-manager x y emp)
  "Returns the closest common manager of the employees x and y"
  (define (employee-by-name x)
    (set-filter
     (match-lambda [(name id age year manager salary) (string=? name x)]) emp))
  (define (employee-managers e)
    (match-let manager* ([(name id age year manager salary) e] [r '()])
               (cond
                 [(string-null? manager) r]
                 [else
                  (match-let ([(em) (employee-by-name manager)])
                    (manager* em (cons manager r)))])))
  (match-let* ([(ex) (employee-by-name x)]
               [(ey) (employee-by-name y)]
               [xm (employee-managers ex)]
               [ym (employee-managers ey)])
    ;; (pp (list xm ym))
    (let common* ([xm xm] [ym ym] [m #f])
      (cond
        [(or (null? xm) (null? ym)) m]
        [(string=? (car xm) (car ym)) (common* (cdr xm) (cdr ym) (car xm))]
        [else m]))))

;; (pp (employee-closest-common-manager "White" "Black" emp))
;; (pp (employee-closest-common-manager "Black" "Wilson" emp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ORDERED PAIR

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data abstraction primitives

(define (make-op x y)
  "Creates orderd pair from the x and y elements"
  (cons x y))

(define (op-left op)
  "Returns the left element of the ordered pair op"
  (car op))

(define (op-right op)
  "Returns the right element of the ordered pair op"
  (cdr op))

(define (op? x)
  "Returns #t if the object x is an ordered pair"
  (pair? x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data abstraction algorithms

(define (cartesian-product x y)
  "Returns the set of Cartesian product of the sets x and y"
  (let outer* ([l x] [os (set-empty)])
    (if [set-empty? l] os
        (let ([le (pick l)])
          (let inner* ([r y] [is (set-empty)])
            (if [set-empty? r]
                (outer* ((exclude le) l) (set-union is os))
                (let* ([re (pick r)] [op (make-op le re)])
                  (inner* ((exclude re) r) (adjoin op is)))))))))

(define (cartesian-product2 x y)
  "Returns the set of Cartesian product of the sets x and y"
  (let* ([ll (set-map (lambda (le) (set-map (lambda (re) (make-op le re)) y)) x)]
         [l (fold append '() ll)])
    (apply make-set l)))

;; (pp (cartesian-product (make-set 'A 'B) (make-set 'a 'b 'c)))
;; (pp (cartesian-product2 (make-set 'A 'B) (make-set 'a 'b 'c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RELATIONS AND FUNCITONS

(define (make-relation . l)
  "Creates a relation from the list l of two element lists"
  (apply make-set (map (lambda (e) (apply make-op e)) l)))

;; (pp (make-relation '(1 2) '(3 4) '(5 6)))

(define (domain r)
  "Returns the domain of the relation r represented as a set of ordered pairs"
  (set-map op-left r))

(define (range r)
  "Returns the rante of the relation r represented as a set of ordered pairs"
  (set-map op-right r))

(define (relation? x)
  "Returns #t if the object x is a relation (a set of ordered pairs)"
  ((set-every op?) x))

;; (pp (relation? (set-empty)))
;; (pp (relation? (make-set (make-op 'a 'b))))
;; (pp (relation? (make-relation '(a b))))
;; (pp (relation? (make-set 'a 'b)))

(define (relation-inverse r)
  "Returns the inverse relation of the relation r"
  (set-map (lambda (op) (make-op (op-right op) (op-left op))) r))

;; (pp (relation-inverse (make-relation '(0 0) '(1 1) '(2 4) '(3 9))))

(define (relation-reflexive? r)
  "Returns #t if the relation r is reflexive (every element of r is related to inself)"
  ((set-every (lambda (x) ((set-any (lambda (y) (equal? (op-left x) (op-right y)))) r))) r))

;; (pp (relation-reflexive? (make-relation '(0 0) '(1 1) '(2 2) '(3 3))))
;; (pp (relation-reflexive? (make-relation '(0 0) '(1 1) '(2 4) '(3 9))))

(define (relation-symmetric? r)
  "Returns #t if the relation r is symmetric (is equal to its inverse relaiton)"
  ((set-equal? r) (relation-inverse r)))

;; (pp (relation-symmetric? (make-relation '(0 0) '(1 1) '(2 2) '(3 3))))
;; (pp (relation-symmetric? (make-relation '(0 0) '(1 1) '(2 4) '(3 9))))

(define (function? r)
  "Returns #t if the relation r represented as a set of ordered pairs is a funciton"
  (or [set-empty? r]
      (let* ([op (pick r)]
             [x (op-left op)]
             [sr (set-filter (lambda (rop) (equal? (op-left rop) x)) r)])
        (and (= (cardinal sr) 1) (function? (set-difference r sr))))))

;; (pp (function? (make-relation '(0 0) '(1 1) '(2 4) '(3 9))))

(define ((function-value r) x)
  "Returns the value of a function represented by the relation r at the argument x"
  (let ([sr (set-filter (lambda (rop) (equal? (op-left rop) x)) r)])
    (when [set-empty? sr] (error "value: function is not defined for:" x))
    (op-right (pick sr))))

;; (pp ((function-value (make-relation '(0 0) '(1 1) '(2 4) '(3 9))) 2))

(define (function-one-to-one? r)
  "Returns #t if the function r is one-to-one"
  (and (function? r) (function? (relation-inverse r))))

;; (pp (function-one-to-one? (make-relation '(0 0) '(1 1) '(2 4) '(3 9))))
;; (pp (function-one-to-one? (make-relation '(-1 1) '(0 0) '(1 1) '(2 4) '(3 9))))
