(define-module
  (sorting-algorithm))

(use-modules
 ((ice-9 pretty-print)
  #:select ((pretty-print . pp))))

(define* (insert-sort-recursive l #:optional (c <=))
  "Sorts a copy of the list l by applying the insert sort algorithm recursively"
  ;; (format #t "sort: ~s\n" l)
  (define (singleton? l)
    (and [pair? l] [null? (cdr l)]))
  (define (insert e l)
    ;; (format #t "insert: ~s ~s\n" e l)
    (cond
      [(null? l) (cons e l)]
      [(c e (car l)) (cons e l)]
      [else (cons (car l) (insert e (cdr l)))]))
  (if [singleton? l] l (insert (car l) (insert-sort-recursive (cdr l) c))))

(pp (insert-sort-recursive '(5 7 3 8 1 9 2 6 4)))
(pp (insert-sort-recursive '(5 7 3 8 1 9 2 6 4) >=))

(define* (insert-sort-iterative v #:optional (c <))
  "Sorts the vector v in place by applying the insert sort algorithm iteratively")
