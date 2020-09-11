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
  (define (insert* e l)
    ;; (format #t "insert: ~s ~s\n" e l)
    (cond
      [(null? l) (cons e l)]
      [(c e (car l)) (cons e l)]
      [else (cons (car l) (insert* e (cdr l)))]))
  (if [singleton? l] l (insert* (car l) (insert-sort-recursive (cdr l) c))))

;; (pp (insert-sort-recursive '(1)))
;; (pp (insert-sort-recursive '(5 7 3 8 1 9 2 6 4)))
;; (pp (insert-sort-recursive '(5 7 3 8 1 9 2 6 4) >=))

(define* (insert-sort-iterative v #:optional (c <=))
  "Sorts the vector v in place by applying the insert sort algorithm iteratively"
  (let ([n (vector-length v)])
    (do ([i 1 (1+ i)]) ([= i n] v)
      (let ([ei (vector-ref v i)])
        ;; (format #t "* [~s] ~s ~s\n" i ei v)
        (let insert* ([j (1- i)])
          (let ([ej (vector-ref v j)])
            ;; (format #t "  > [~s] ~s ~s\n" j ej v)
            (cond
              [(c ej ei) (vector-set! v (1+ j) ei)]
              [(zero? j) (vector-set! v (1+ j) ej) (vector-set! v j ei)]
              [else (vector-set! v (1+ j) ej) (insert* (1- j))])))))))

;; (pp (insert-sort-iterative (vector 1)))
;; (pp (insert-sort-iterative (vector 5 7 3 8 1 9 2 6 4)))
;; (pp (insert-sort-iterative (vector 5 7 3 8 1 9 2 6 4) >=))
