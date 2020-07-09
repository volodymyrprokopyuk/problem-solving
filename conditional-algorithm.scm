(define-module
  (conditional-algorithm)
  #:export
  (at-least at-most and* or* xor equiv make-domain make-truth-table format-truth-table))

(use-modules
 ((ice-9 pretty-print)
  #:select ((pretty-print . pp))))

(define (at-least match? l)
  "Returns the first element from the list l that matches, otherwise #f"
  (let at-least* ([l l])
    (cond
      [(null? l) #f]
      [(match? (car l)) (car l)]
      [else (at-least* (cdr l))])))

(define (at-most match? l)
  "Returns the first and only element from the list l that matches, otherwise #f"
  (let at-most* ([l l] [found #f])
    (cond
      [(and (null? l) (not found)) #f]
      [(null? l) found]
      [(and (match? (car l)) (not found)) (at-most* (cdr l) (car l))]
      [(match? (car l)) #f]
      [else (at-most* (cdr l) found)])))

(define and*
  (case-lambda
    "Returns #t if all arguments are truthy, othewise #f (eager logical AND function)"
    [() #t]
    [(a) (if a #t #f)]
    [(a b) (if a (if b #t #f) #f)]
    [(a b . r) (apply and* (if a (if b #t #f) #f) r)]))

(define or*
  (case-lambda
    "Returns #f if all arguments are falsy, othewise #t (eager logical OR function)"
    [() #f]
    [(a) (if a #t #f)]
    [(a b) (cond [a #t] [b #t] [else #f])]
    [(a b . r) (apply or* (cond [a #t] [b #t] [else #f]) r)]))

;; Complement to equiv
(define (xor . args)
  "Returns #f if all arguments are equal, otherwise #t (exclusive OR XOR)"
  (and (apply or* args) (not (apply and* args))))

;; Complement to xor
(define (equiv . args)
  "Returns #t if all arguments are equal, otherwise #f (equivalence)"
  (or (apply and* args) (apply and* (map not args))))

;; Non-tail recursive solution
(define (make-domain2 digits dimension)
  "Builds all combinations of digits for a dimension"
  ;; There is an empty combination for a dimension 0
  (if [zero? dimension] '(())
      ;; Combine all combinations
      (apply append
             ;; For each digit from digits
             (map (lambda (d)
                    ;; Prepend the digit to each combination
                    ;; for a smaller by one dimension
                    (map (lambda (sd) (cons d sd))
                         (make-domain digits (1- dimension))))
                  digits))))

;; Tail-recursive solution
(define (make-domain digits dimension)
  "Builds all combinations of digits for a dimension"
  (let make-domain* ([dim dimension] [r '(())])
    (if [zero? dim] r
        (make-domain*
         (1- dim)
         (apply append
                (map (lambda (d)
                       (map (lambda (sr) (cons d sr)) r))
                     digits))))))

(define (make-truth-table f arity)
  "Builds a truth table of the function f for the arity"
  (let ([domain (make-domain '(#f #t) arity)])
    (map (lambda (args) (cons args (apply f args))) domain)))

(define (format-truth-table t)
  "Formats the truth table t into a string"
  (apply string-append (map (lambda (e) (format #f "~s = ~s\n" (car e) (cdr e))) t)))

;; (pp "Negation NOT")
;; (pp (format-truth-table (make-truth-table not 1)) #:display? #t)
;; (pp "Conjunction AND")
;; (pp (format-truth-table (make-truth-table and* 2)) #:display? #t)
;; (pp "Disjunction OR")
;; (pp (format-truth-table (make-truth-table or* 2)) #:display? #t)
;; (pp "Exclusive OR XOR")
;; (pp (format-truth-table (make-truth-table xor 2)) #:display? #t)
;; (pp "Equivalence")
;; (pp (format-truth-table (make-truth-table equiv 2)) #:display? #t)
