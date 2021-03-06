(define-module
 (recursive-algorithm)
  #:export
  (factorial fibonacci meven? modd? hanoi power-set permute umap mmap power
             combinations-number minimax string-symmetric? last-element
             remove-last-element remove-first-occurrence remove-last-occurrence
             substitute-first-occurrence interleave-first-duplication all-same?
             swap-occurences harmonic-sum dot-product append2 merge-sorted
             count-nested remove-nested reverse-nested substitute-nested sum-nested
             nested-depth flatten-nested remove-nested-first count-open-parentheses
             count-nested-occurences leftmost-atomic rightmost-atomic compose2 compose3
             round-number prime? flat-filter deep-filter))

(use-modules
 (ice-9 curried-definitions)
 (srfi srfi-1) ;; List library
 (srfi srfi-42) ;; Comprehensions
 ((ice-9 pretty-print)
  #:select ((pretty-print . pp))))

(define (factorial n)
  "Calculates the factorial of the number n"
  (let factorial* ([n n] [r 1])
    ;; Direct recutsion
    (if (zero? n) r (factorial* (1- n) (* r n)))))

(define (fibonacci n)
  "Calculates the Fibonacci sequence value for the position n"
  (let fibonacci* ([n n] [a -1] [b 1])
    ;; Tail recursion
    (if (zero? n) (+ a b) (fibonacci* (1- n) b (+ a b)))))

(define (meven? n)
  "Returns true if the number n is even"
  ;; Indirect recursion
  (if (zero? n) #t (modd? (1- n))))

(define (modd? n)
  "Returns true if the number n is odd"
  ;; Indirect recursion
  (if (zero? n) #f (meven? (1- n))))

;; Tower of Hanoi (side effecting solution)
(define (pp-hanoi n source target spare)
  "Finds the solution for the Tower of Hanoi problem for number of disks n"
  (cond
    [(= n 1) (pp (cons source target))]
    [else
     ;; Move (n - 1) disks from source to spare
     (pp-hanoi (1- n) source spare target)
     ;; Move the biggest last disk from source to target
     (pp-hanoi 1 source target spare)
     ;; Mothe the previously moded (n - 1) disks from spare to target
     (pp-hanoi (1- n) spare target source)]))

;; (pp-hanoi 1 'source 'target 'spare) (newline)
;; (pp-hanoi 2 'source 'target 'spare) (newline)
;; (pp-hanoi 3 'source 'target 'spare)

;; Tower of Hanoi (functional solution)
(define (hanoi n source target spare)
  "Finds the solution for the Tower of Hanoi problem for number of disks n"
  (cond
    [(zero? n) '()]
    [else
     ;; Combine all moves
     (append
      ;; Move (n - 1) disks from source to spare
      (hanoi (1- n) source spare target)
      ;; Move the biggest last disk from source to target
      (list (cons source target))
      ;; Mothe the previously moded (n - 1) disks from spare to target
      (hanoi (1- n) spare target source))]))

(define (power-set l)
  "Builds a set of all subsets of the list l including the empty set"
  ;; Empty set is a subset of itself
  (if [null? l] '(())
      (let ([s (power-set (cdr l))])
        ;; Combine the power set of all but the first element
        ;; with itself prepended with the first element
        (append s (map (lambda (ss) (cons (car l) ss)) s)))))

(define (permute l)
  "Builds all permutations of the list l"
  (if [null? l] '(())
      ;; Combine all permutations
      (apply append
             ;; For each element in the list
             (map (lambda (e)
                    ;; Prepend the element to a subset of all permutations
                    ;; without this element
                    (map (lambda (p) (cons e p)) (permute (delete e l))))
                  l))))

(define (umap f l)
  "Maps the function f of one argument over the list l"
  (if [null? l] '()
      (cons (f (car l)) (umap f (cdr l)))))

(define (mmap f l . ll)
  "Maps the function f of multiple arguments over the list l"
  (if [null? l] '()
      ;; Construct list from results of applying function f
      ;; to first elements of all lists
      (cons (apply f (cons (car l) (umap car ll)))
            ;; Recursively apply function f to the corresponding elements of all lists
            (apply mmap (cons f (cons (cdr l) (umap cdr ll)))))))

(define (power x n)
  "Returns power x to n"
  (if (positive? n)
      (let power* ([i n] [r 1])
        (if [zero? i] r (power* (1- i) (* x r))))
      (let power* ([i n] [r 1])
        (if [zero? i] r (power* (1+ i) (* (/ 1 x) r))))))

(define (combinations-number n k)
  "Returns the number of combination of k from n without repetition"
  (if [or (zero? k) (= k n)] 1
      (+ (combinations-number (1- n) k) (combinations-number (1- n) (1- k)))))

(define (combinations-number2 n k)
  "Returns the number of combination of k from n without repetition"
  (/ (factorial n) (* (factorial k) (factorial (- n k)))))

(define (minimax l compare)
  "Returns minimum or maximum element of the list"
  (let minimax* ([l l] [r #f])
    (cond
      [(null? l) r]
      [(not r) (minimax* (cdr l) (car l))]
      [(compare (car l) r) (minimax* (cdr l) (car l))]
      [else (minimax* (cdr l) r)])))

(define (string-symmetric? s)
  "Returns #t on symmetric string"
  (let symm* ([ss s] [st '()] [ac 'push])
    (cond
      ;; At the end of a symmetric string stack should be empty
      [(string-null? ss) (null? st)]
      ;; Push the first character into the stack
      [(null? st)
       (symm* (string-drop ss 1) (cons (string-ref ss 0) st)
              (if (= (- (/ (string-length s) 2) (length st)) 1) 'pop 'push))]
      ;; Push the first half of the string into the stack
      [(and (eq? ac 'push) (< (length st) (/ (string-length s) 2)))
       (symm* (string-drop ss 1) (cons (string-ref ss 0) st)
              (if (= (- (/ (string-length s) 2) (length st)) 1) 'pop 'push))]
      ;; Check the second half of the string with the stack content
      [(and (eq? ac 'pop) (eqv? (string-ref ss 0) (car st)))
       (symm* (string-drop ss 1) (cdr st) 'pop)]
      ;; Short-circuit on the first unmatch
      [else #f])))

(define (member-element? e l)
  "Returns a sublist of the list l whose car is equal? to the element e, otherwise false"
  (cond
    [(null? l) #f]
    [(equal? e (car l)) l]
    [else (member-element? e (cdr l))]))

;; (pp (member-element? 'b '(a b c d)))

(define (last-element l)
  "Returns last element from a list"
  (if [and (pair? l) (null? (cdr l))] (car l) (last-element (cdr l))))

;; (pp (last-element '(a b c d)))

(define (remove-last-element l)
  "Removes last element from the list l"
  (let remove* ([l l] [r '()])
    (cond
      [(or (null? l) (null? (cdr l))) r]
      [(null? (cddr l)) (reverse (cons (car l) r))]
      [else (remove* (cdr l) (cons (car l) r))])))

;; (pp (remove-last-element '()))
;; (pp (remove-last-element '(a)))
;; (pp (remove-last-element '(a b)))
;; (pp (remove-last-element '(a b c d e)))

(define (remove-first-occurrence e l)
  "Removes the first occurrence of the element e in the list l"
  (let remove* ([l l] [r '()])
    (cond
      [(null? l) r]
      [(equal? e (car l)) (append (reverse r) (cdr l))]
      [else (remove* (cdr l) (cons (car l) r))])))

;; (pp (remove-first-occurrence 'b '(a b c b d)))

(define (remove-last-occurrence e l)
  "Removes the last occurrence of the element e in the list"
  (reverse (remove-first-occurrence e (reverse l))))

;; (pp (remove-last-occurrence 'b '(a b c b d)))

(define (substitute-first-occurrence a b l)
  "Subsitutes the first occurrence of the element a with the element b in the list"
  (let subs* ([l l] [r '()])
    (cond
      [(null? l) r]
      [(equal? a (car l)) (append (reverse (cons b r)) (cdr l))]
      [else (subs* (cdr l) (cons (car l) r))])))

;; (pp (substitute-first-occurrence 'b 'B '(a b c b d)))

(define (interleave-first-duplication a b l)
  "Interleaves the first duplication of a with a b a in the list"
  (let interl* ([l l] [r '()])
    (cond
      [(null? l) l]
      [(null? (cdr l)) (reverse (cons (car l) r))]
      [(equal? (car l) (cadr l))
       (append (reverse (cons (car l) (cons b (cons (cadr l) r)))) (cddr l))]
      [else (interl* (cdr l) (cons (car l) r))])))

;; (pp (interleave-first-duplication 'b 'B '()))
;; (pp (interleave-first-duplication 'b 'B '(a)))
;; (pp (interleave-first-duplication 'b 'B '(b b)))
;; (pp (interleave-first-duplication 'b 'B '(a b b c b b d)))
;; (pp (interleave-first-duplication 'b 'B '(a b c b d)))

(define (all-same? l)
  "Returns #t if all elements in the list l are the same"
  (cond
    [(or (null? l) (null? (cdr l))) #t]
    [(not (equal? (car l) (cadr l))) #f]
    [else (all-same? (cdr l))]))

;; (pp (all-same? '()))
;; (pp (all-same? '(a)))
;; (pp (all-same? '(a a)))
;; (pp (all-same? '(a a a)))
;; (pp (all-same? '(a b a)))
;; (pp (all-same? '((a b) (a b) (a b))))

(define (swap-occurences a b l)
  "Swaps occurrences of a with b and viceversa in the list"
  (define (swap-element e)
    (cond
      [(equal? e a) b]
      [(equal? e b) a]
      [else e]))
  (let swap* ([l l] [r '()])
    (if [null? l] (reverse r)
        (swap* (cdr l) (cons (swap-element (car l)) r)))))

;; (pp (swap-occurences 'b 'a '(a b c b d)))

(define (harmonic-sum n)
  "Returns the sum of harmonic sequence"
  #;(fold-ec 0 (:range i 1 n) (/ 1 i) +)
  (let hsum* ([n n] [r 0])
    (if [zero? n] r (hsum* (1- n) (+ (/ 1 n) r)))))

;; (pp (harmonic-sum 10))

(define (dot-product k l)
  "Returns sum of products of corresponding elements from k and l lists"
  (let dotp* ([k k] [l l] [r 0])
    (if [or (null? k) (null? l)] r
        (dotp* (cdr k) (cdr l) (+ (* (car k) (car l)) r)))))

;; (pp (dot-product '(1 2 3) '(4 5 6)))

(define (append2 k l)
  "Appends two lists k and l into one single list"
  (let append* ([k (reverse k)] [r l])
    (if [null? k] r (append* (cdr k) (cons (car k) r)))))

;; (pp (append2 '(a b) '(c d)))

(define (merge-sorted k l)
  "Merges two sorted lists k and l in one sorted list"
  (let merge* ([k k] [l l] [r '()])
    (cond
      [(null? k) (append (reverse r) l)]
      [(null? l) (append (reverse r) k)]
      [(< (car k) (car l)) (merge* (cdr k) l (cons (car k) r))]
      [else (merge* k (cdr l) (cons (car l) r))])))

;; (pp (merge-sorted '(1 7 9 11) '(2 4 6 12 14 16)))

(define (count-nested l)
  "Returns the number of nested atomic (non-pair) elements in the list"
  (let count* ([l l] [r 0])
    (cond
      [(null? l) r]
      [(pair? (car l)) (count* (cdr l) (+ (count* (car l) 0) r))]
      [else (count* (cdr l) (1+ r))])))

;; (pp (count-nested '(a (b c) () (d (e f (g))))))

(define (remove-nested e l)
  "Removes all nested occurrences of the element e in the list l"
  (let remove* ([l l] [r '()])
    (cond
      [(null? l) (reverse r)]
      [(pair? (car l)) (remove* (cdr l) (cons (remove* (car l) '()) r))]
      [(equal? (car l) e) (remove* (cdr l) r)]
      [else (remove* (cdr l) (cons (car l) r))])))

;; (pp (remove-nested 'a '(a b (c a) (d (e a b (a))))))

(define (reverse-nested l)
  "Reverses the list l including nested sublists"
  (let reverse* ([l l] [r '()])
    (cond
      [(null? l) r]
      [(pair? (car l)) (reverse* (cdr l) (cons (reverse* (car l) '()) r))]
      [else (reverse* (cdr l) (cons (car l) r))])))

;; (pp (reverse-nested '(a b (c a) (d (e a b (a))))))

(define (substitute-nested a b l)
  "Subsitutes nested occurences of a with b in the list l"
  (let subs* ([l l] [r '()])
    (cond
      [(null? l) (reverse r)]
      [(pair? (car l)) (subs* (cdr l) (cons (subs* (car l) '()) r))]
      [(equal? (car l) a) (subs* (cdr l) (cons b r))]
      [else (subs* (cdr l) (cons (car l) r))])))

;; (pp (substitute-nested 'a 'A '(a b (c a) (d (e a b (a))))))

(define (sum-nested l)
  "Adds all elements of the list l including nested elements"
  (let sum* ([l l] [r 0])
    (cond
      [(null? l) r]
      [(pair? (car l)) (sum* (cdr l) (+ (sum* (car l) 0) r))]
      [else (sum* (cdr l) (+ (car l) r))])))

;; (pp (sum-nested '(1 2 (3 4) (5 (6 (7 9))))))

(define (nested-depth l)
  "Returns the depth of the nested list l"
  (let depth* ([l l] [r 0])
    (if [pair? l] (depth* (cdr l) (max (1+ (depth* (car l) 0)) r)) r)))

;; (pp (nested-depth '()))
;; (pp (nested-depth '(a b)))
;; (pp (nested-depth '(a b (c))))
;; (pp (nested-depth '(a b (c d (e f)))))

(define (flatten-nested l)
  "Flattens the nested list l"
  (reverse
   (let flatten* ([l l] [r '()])
     (cond
       [(null? l) r]
       [(pair? (car l)) (flatten* (cdr l) (append (flatten* (car l) '()) r))]
       [else (flatten* (cdr l) (cons (car l) r))]))))

;; (pp (flatten-nested '(a b c d)))
;; (pp (flatten-nested '(a b (c d e) f g)))
;; (pp (flatten-nested '(a b (c d (e f) g h) i j)))

(define (remove-nested-first e l)
  "Removes first occurrence of eleemnt e in the list l including nested sublists"
   (let remove* ([l l] [r '()])
     (cond
       [(null? l) (reverse r)]
       [(pair? (car l))
        (let ([cl (remove* (car l) '())])
          (if [equal? cl (car l)]
              (remove* (cdr l) (cons cl r))
              (append (reverse (cons cl r)) (cdr l))))]
       [(equal? (car l) e) (append (reverse r) (cdr l))]
       [else (remove* (cdr l) (cons (car l) r))])))

;; (pp (remove-nested-first 'B '(a b c)))
;; (pp (remove-nested-first 'c '(a b c d e c f)))
;; (pp (remove-nested-first 'd '(a b (c d e d f))))
;; (pp (remove-nested-first 'f '(a b (c d (e f g f h) i f j))))

(define (count-open-parentheses l)
  "Counts number of parentheses in the list l including nested sublists"
  (let count* ([l l] [r 1])
    (cond
      [(null? l) r]
      [(null? (car l)) (count* (cdr l) (1+ r))]
      [(pair? (car l)) (count* (cdr l) (+ (count* (car l) 1) r))]
      [else (count* (cdr l) r)])))

;; (pp (count-open-parentheses '()))
;; (pp (count-open-parentheses '(a b)))
;; (pp (count-open-parentheses '(a b (c d))))
;; (pp (count-open-parentheses '(a () b (c () d (e ())))))

(define (count-nested-occurences e l)
  "Returns the number of all occurences of the element e in the list l"
  (let count* ([l l] [r 0])
    (cond
      [(null? l) r]
      [(pair? (car l)) (count* (cdr l) (+ (count* (car l) 0) r))]
      [(equal? (car l) e) (count* (cdr l) (1+ r))]
      [else (count* (cdr l) r)])))

;; (pp (count-nested-occurences 'a '(a b (c a (d a) (a) e) f)))

(define (leftmost-atomic l)
  "Returns the leftmost atomic element"
  (cond
    [(null? l) #f]
    [(pair? (car l)) (leftmost-atomic (car l))]
    [else (car l)]))

;; (pp (leftmost-atomic '()))
;; (pp (leftmost-atomic '(a b c)))
;; (pp (leftmost-atomic '((() a) b c)))

(define (rightmost-atomic l)
  "Returns the rightmost atomic element"
  (let rightmost* ([l l] [r #f])
    (cond
      [(null? l) r]
      [(pair? (car l)) (rightmost* (cdr l) (rightmost* (car l) #f))]
      [else (rightmost* (cdr l) (car l))])))

;; (pp (rightmost-atomic '()))
;; (pp (rightmost-atomic '(a b c)))
;; (pp (rightmost-atomic '(a (b c))))
;; (pp (rightmost-atomic '(a (b (c)))))
;; (pp (rightmost-atomic '(a (b (c) d))))
;; (pp (rightmost-atomic '(a (b (c) d) e f)))
;; (pp (rightmost-atomic '(a (b (c) d) e f (g h))))

(define (compose2 . fs)
  "Returns composition of the fs functions"
  (lambda (x) (fold-right (lambda (f b) (f b)) x fs)))

;; (pp ((compose2 (lambda (x) (/ x 3)) (lambda (x) (* x 2)) 1+) 1))

(define (compose3 . fs)
  "Returns composition of the fs functions"
  (lambda (x)
    (let compose* ([fs (reverse fs)] [r x])
      (if [null? fs] r (compose* (cdr fs) ((car fs) r))))))

;; (pp ((compose3 (lambda (x) (/ x 3)) (lambda (x) (* x 2)) 1+) 1))

(define ((round-number n) x)
  (let ([k (expt 10 n)])
    (/ (round (* (exact->inexact x) k)) k)))

#;(let ([round-3 (round-number 3)])
  (pp (round-3 (sqrt 2)))
  (pp (round-3 (sqrt 3)))
  (pp (round-3 2/3)))

(define (prime? x)
  (cond
    [(= x 2) #t]
    [(or (<= x 1) (even? x)) #f]
    [else
     (let ([r (floor (sqrt x))])
       (let prime* ([d (if [even? r] (1- r) r)])
         (cond
           [(< d 3) #t]
           [(zero? (remainder x d)) #f]
           [else (prime* (- d 2))])))]))

;; (pp (map (lambda (x) (cons x (prime? x))) (iota 20)))

(define (flat-filter p l)
  "Filters top-level elements of the list l with the predicate p"
  (let filter* ([l l] [r '()])
    (cond
      [(null? l) (reverse r)]
      [(p (car l)) (filter* (cdr l) (cons (car l) r))]
      [else (filter* (cdr l) r)])))

;; (pp (flat-filter even? '(1 2 3 4 5 6)))

(define (deep-filter p l)
  "Filters all elements of the nested list l with the predicate p"
  (let filter* ([l l] [r '()])
    (cond
      [(null? l) (reverse r)]
      [(pair? (car l))
       (filter* (cdr l) (cons (filter* (car l) '()) r))]
      [(p (car l)) (filter* (cdr l) (cons (car l) r))]
      [else (filter* (cdr l) r)])))

;; (pp (deep-filter even? '(1 2 3 (4 5 6 7 (8 9 10 11) (12 13 14 15) 16 17 19) 19 20 21)))

(define (pascal-triangle n k)
  "Returns the value of the Pascal triangle at n row and k column"
  (cond
    [(or (zero? k) (= n k)) 1]
    [else (+ (pascal-triangle (1- n) (1- k)) (pascal-triangle (1- n) k))]))

;; (pp (list-ec (:range i 0 1) (pascal-triangle 0 i)))
;; (pp (list-ec (:range i 0 2) (pascal-triangle 1 i)))
;; (pp (list-ec (:range i 0 3) (pascal-triangle 2 i)))
;; (pp (list-ec (:range i 0 4) (pascal-triangle 3 i)))
;; (pp (list-ec (:range i 0 5) (pascal-triangle 4 i)))
;; (pp (list-ec (:range i 0 6) (pascal-triangle 5 i)))
;; (pp (list-ec (:range i 0 7) (pascal-triangle 6 i)))
