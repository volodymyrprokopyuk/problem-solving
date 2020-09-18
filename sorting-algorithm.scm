(define-module
  (sorting-algorithm))

(use-modules
 (ice-9 receive)
 ((ice-9 pretty-print)
  #:select ((pretty-print . pp))))

(define (singleton? l)
  "Returns #t if the list l has only one element"
  (and [pair? l] [null? (cdr l)]))

(define* (insertsort l #:optional (c <=))
  "Sorts O(n2) a copy of the list l by applying the insert sort algorithm recursively"
  ;; (format #t "sort: ~s\n" l)
  (define (insert* e l)
    ;; (format #t "insert: ~s ~s\n" e l)
    (cond
      [(null? l) (cons e l)]
      [(c e (car l)) (cons e l)]
      [else (cons (car l) (insert* e (cdr l)))]))
  (if [singleton? l] l (insert* (car l) (insertsort (cdr l) c))))

;; (pp (insertsort '(1)))
;; (pp (insertsort '(1 1)))
;; (pp (insertsort '(5 7 3 8 1 9 2 6 4)))
;; (pp (insertsort '(5 7 3 8 1 9 2 6 4) >=))

(define* (vector-insertsort! v #:optional (c <=))
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

;; (pp (vector-insertsort! (vector 1)))
;; (pp (vector-insertsort! (vector 1 1)))
;; (pp (vector-insertsort! (vector 5 7 3 8 1 9 2 6 4)))
;; (pp (vector-insertsort! (vector 5 7 3 8 1 9 2 6 4) >=))

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

(define* (mergesort l #:optional (c <=))
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

;; (pp (mergesort '(1)))
;; (pp (mergesort '(1 1)))
;; (pp (mergesort '(5 7 3 8 1 9 2 6 4)))
;; (pp (mergesort '(5 7 3 8 1 9 2 6 4) >=))

(define* (vector-group-successive v #:optional (c <=))
  "Returns a list of successive groups identified by the indices as per comparator c"
  (let ([n (vector-length v)])
    (let group* ([i 0] [a #f] [l #f] [r '()])
      (cond
        [(= i n) (reverse (cons (list a i) r))]
        [(not a) (group* (1+ i) i (vector-ref v i) r)]
        [(c l (vector-ref v i)) (group* (1+ i) a (vector-ref v i) r)]
        [else (group* (1+ i) i (vector-ref v i) (cons (list a i) r))]))))

;; (pp (vector-group-successive (vector 5 7 3 8 1 9 2 6 4)))

(define* (vector-merge-pair! v x y #:optional (c <=))
  "Merges in place the ordered subvectors of the vector v identified by the x and y indices"
  (let* ([xa (car x)] [xb (cadr x)] [ya (car y)] [yb (cadr y)]
                      [n (- yb xa)][sv (make-vector n)])
    (let merge* ([i xa] [j ya] [k 0])
      ;; (format #t "  > [~s ~s ~s] [~s ~s ~s] ~s\n" xa i xb ya j yb k)
      (cond
        [(and [= i xb] [= j yb])
         (do ([k 0 (1+ k)] [i xa (1+ i)]) ([= k n] (list xa yb))
           (vector-set! v i (vector-ref sv k)))]
        [(= i xb) (vector-set! sv k (vector-ref v j)) (merge* i (1+ j) (1+ k))]
        [(= j yb) (vector-set! sv k (vector-ref v i)) (merge* (1+ i) j (1+ k))]
        [(c (vector-ref v i) (vector-ref v j))
         (vector-set! sv k (vector-ref v i)) (merge* (1+ i) j (1+ k))]
        [else (vector-set! sv k (vector-ref v j)) (merge* i (1+ j) (1+ k))]))))

;; (let ([v (vector 5 7 3 8 1 9 2 6 4)])
;;   (pp (vector-merge-pair! v '(0 2) '(2 4))) (pp v))
;; (let ([v (vector 1 2 3 5 6 7 8 9 4)])
;;   (pp (vector-merge-pair! v '(0 8) '(8 9))) (pp v))

(define* (vector-mergesort! v #:optional (c <=))
  "Sorts O(nlogn) the vector v in place by applying the merge sort algorithm iteratively"
  (let merge* ([g (vector-group-successive v c)] [r '()])
    ;; (format #t "* ~s ~s ~s\n" g r v)
    (cond
      [(and [singleton? g] [null? r]) v]
      [(and [null? g] [singleton? r]) v]
      [(null? g) (merge* (reverse r) '())]
      [(singleton? g) (merge* (reverse (cons (car g) r)) '())]
      [else
       (let ([mg (vector-merge-pair! v (car g) (cadr g) c)])
         (merge* (cddr g) (cons mg r)))])))

;; (pp (vector-mergesort! (vector 1)))
;; (pp (vector-mergesort! (vector 1 1)))
;; (pp (vector-mergesort! (vector 5 7 3 8 1 9 2 6 4)))
;; (pp (vector-mergesort! (vector 5 7 3 8 1 9 2 6 4) >=))

(define* (quicksort l #:optional (c <=))
  "Sorts O(nlogn) a copy of the list l by applying the quick sort algorithm recursively"
  (if (or [null? l] [singleton? l]) l
      (let sort* ([p (car l)] [l (cdr l)] [ll '()] [rr '()])
        (cond
          [(null? l) (append (quicksort ll c) (cons p (quicksort rr c)))]
          [(c (car l) p) (sort* p (cdr l) (cons (car l) ll) rr)]
          [else (sort* p (cdr l) ll (cons (car l) rr))]))))

;; (pp (quicksort '(1)))
;; (pp (quicksort '(1 1)))
;; (pp (quicksort '(5 7 3 8 1 9 2 6 4)))
;; (pp (quicksort '(5 7 3 8 1 9 2 6 4) >=))

(define (vector-swap! v i j)
  "Swaps in place the elements with the indices i and j of the vector v"
  (let ([ei (vector-ref v i)] [ej (vector-ref v j)])
    (format #t "  <> ~s\n    [~s]~s <=> [~s]~s\n" v i ei j ej)
    (vector-set! v i ej) (vector-set! v j ei)))

(define* (vector-quicksort! v #:optional (c <=))
  "Sorts O(nlogn) the vector v in place by applying the quick sort algorithm iteratively"
  (define (partition! a b)
    (let* ([k (floor (/ (+ a b) 2))]
           [p (vector-ref v k)])
      (format #t "* [~s, ~s] [~s]~s ~s\n" a b k p v)
      (let partition!* ([ii a] [jj b])
        (let ([i (do ([i ii (1+ i)]) ([c p (vector-ref v i)] i))]
              [j (do ([j jj (1- j)]) ([c (vector-ref v j) p] j))])
          (cond
            [(>= i j) (format #t "  _| ~s\n" j) j]
            [else (vector-swap! v i j) (partition!* (1+ i) (1- j))])))))
  (let ([n (vector-length v)])
    (cond
      [(or [zero? n] [= n 1]) v]
      [else
       (let sort!* ([a 0] [b (1- n)])
         (cond
           [(< a b)
            (let ([k (partition! a b)])
              (sort!* a k) (sort!* (1+ k) b))]
           [else v]))])))

;; (pp (vector-quicksort! (vector 1)))
;; (pp (vector-quicksort! (vector 1 1)))
;; (pp (vector-quicksort! (vector 5 7 3 8 1 9 2 6 4)))
;; (pp (vector-quicksort! (vector 5 7 3 8 1 9 2 6 4) >=))
