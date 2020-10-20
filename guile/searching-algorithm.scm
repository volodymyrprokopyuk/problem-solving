(define-module
  (searching-algorithm))

(use-modules
 ((ice-9 pretty-print)
  #:select ((pretty-print . pp))))

(define (linear-search l e)
  "Performs the linear search O(n) on the list l for the element e"
  (let search* ([l l] [i 0])
    (cond
      [(null? l) -1]
      [(equal? (car l) e) i]
      [else (search* (cdr l) (1+ i))])))

;; (pp (linear-search '() 2))
;; (pp (linear-search '(6 4 1 3 2 5) 2))
;; (pp (linear-search '(6 4 1 3 2 5) 20))
;; (pp (linear-search '("Vlad" "Lana" "Home" "Nature") "Lana"))

(define (vector-linear-search v e)
  "Performs the linear search O(n) on the vector v for the element e"
  (let ([n (vector-length v)])
    (let search* ([i 0])
      (cond
        [(= i n) -1]
        [(equal? (vector-ref v i) e) i]
        [else (search* (1+ i))]))))

;; (pp (vector-linear-search #() 2))
;; (pp (vector-linear-search #(6 4 1 3 2 5) 2))
;; (pp (vector-linear-search #(6 4 1 3 2 5) 20))
;; (pp (vector-linear-search #("Vlad" "Lana" "Home" "Nature") "Lana"))

(define* (vector-binary-search v e #:optional (c <=))
  "Performs the binary search O(logn) on the vector v for the element e"
  (let ([n (vector-length v)])
    (cond
      [(zero? n) -1]
      [else
       (let search* ([a 0] [b n])
         (cond
           [(>= a b) -1]
           [else
            (let* ([i (floor (/ (+ a b) 2))] [m (vector-ref v i)])
              ;; (format #t "[~s, ~s] ~s [~s]\n" a b m i)
              (cond
                [(equal? m e) i]
                [(c m e) (search* (1+ i) b)]
                [else (search* a i)]))]))])))

;; (pp (vector-binary-search #() 2))
;; (pp (vector-binary-search #(1 2 3 4 5 6) 2))
;; (pp (vector-binary-search #(1 2 3 4 5 6) 5))
;; (pp (vector-binary-search #(1 2 3 4 5 6) 20))
;; (pp (vector-binary-search #("Home" "Lana" "Nature" "Vlad") "Lana" string<=?))
