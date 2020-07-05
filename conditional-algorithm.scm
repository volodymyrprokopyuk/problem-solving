(define-module
  (conditional-algorithm)
  #:export (at-least at-most))

(use-modules
 ((ice-9 pretty-print)
  #:select ((pretty-print . pp))))

(define (at-least match? l)
  "Returns the first list element that matches, otherwise #f"
  (let at-least* ([l l])
    (cond
      [(null? l) #f]
      [(match? (car l)) (car l)]
      [else (at-least* (cdr l))])))

(define (at-most match? l)
  "Returns the first and only list element that matches, otherwise #f"
  (let at-most* ([l l] [found #f])
    (cond
      [(and (null? l) (not found)) #f]
      [(and (null? l) found) found]
      [(and (match? (car l)) (not found)) (at-most* (cdr l) (car l))]
      [(and (match? (car l)) found) #f]
      [else (at-most* (cdr l) found)])))
