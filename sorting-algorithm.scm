(define-module
  (sorting-algorithm))

(use-modules
 ((ice-9 pretty-print)
  #:select ((pretty-print . pp))))

(define (singleton? l)
  "Returns #t if the list l has only one element"
  (and [pair? l] [null? (cdr l)]))

(define* (insert-sort-recursive l #:optional (c <=))
  "Sorts O(n2) a copy of the list l by applying the insert sort algorithm recursively"
  ;; (format #t "sort: ~s\n" l)
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
  "Sorts O(n2) the vector v in place by applying the insert sort algorithm iteratively"
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

(define* (group-successive l #:optional (c <=))
  "Groups successive elements in the list l as per the comparator c"
  (reverse
   (let group* ([l l] [g '()] [r '()])
     (cond
       [(null? l) (if [null? g] r (cons (reverse g) r))]
       [(null? g) (group* (cdr l) (cons (car l) g) r)]
       [(c (car g) (car l)) (group* (cdr l) (cons (car l) g) r)]
       [else (group* (cdr l) (list (car l)) (cons (reverse g) r))]))))

;; (pp (group-successive '(1 2 4 3 5 7 6 9 8)))
;; (pp (group-successive '(5 7 3 8 1 9 2 6 4)))

(define* (merge-pair x y #:optional (c <=))
  "Merges the ordered lists x and y as per comparator c"
  (reverse
   (let merge* ([x x] [y y] [r '()])
     (cond
       [(null? x) (append (reverse y) r)]
       [(null? y) (append (reverse x) r)]
       [(c (car x) (car y)) (merge* (cdr x) y (cons (car x) r))]
       [else (merge* x (cdr y) (cons (car y) r))]))))

;; (pp (merge-pair '(1 3 5 6 7 8 9) '(2 4 8 9 10 11)))

(define* (merge-sort-recursive l #:optional (c <=))
  "Sorts O(nlogn) a copy of the list l by applying the merge sort algorithm recursively"
  (let merge* ([g (group-successive l c)] [r '()])
    ;; (format #t "~s -> ~s\n" g r)
    (cond
      [(and (singleton? g) (null? r)) (car g)]
      [(and (null? g) (singleton? r)) (car r)]
      [(null? g) (merge* r '())]
      [(singleton? g) (merge* (cons (car g) r) '())]
      [else
       (let ([m (merge-pair (car g) (cadr g) c)])
         (merge* (cddr g) (cons m r)))])))

;; (pp (merge-sort-recursive '(1)))
;; (pp (merge-sort-recursive '(5 7 3 8 1 9 2 6 4)))
;; (pp (merge-sort-recursive '(5 7 3 8 1 9 2 6 4) >=))
