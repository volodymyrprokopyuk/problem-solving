(define-module
  (vector-algorithm)
  #:export (successive-powers vector-linear-search))

(use-modules
 (ice-9 curried-definitions)
 (srfi srfi-1) ;; List library
 ((ice-9 pretty-print)
  #:select ((pretty-print . pp))))

(define ((successive-powers b) n)
  "Returns a vector of size n of successive powers of the base b starting from 0"
  (let ([v (make-vector n)])
    (do ([i 0 (1+ i)]) ([= i n] v) (vector-set! v i (expt b i)))))

;; (pp ((successive-powers 2) 9))
;; (pp ((successive-powers 3) 9))

(define (vector-linear-search v x)
  "Returns the index of the first occurrence of the element x in the vector v
   or -1 otherwise"
  (let ([n (vector-length v)])
    (do ([i 0 (1+ i)]) ((or [= i n] [equal? (vector-ref v i) x]) (if [= i n] -1 i)))))

;; (pp (vector-linear-search #(1 2 3 4 5) 4))
;; (pp (vector-linear-search #(1 2 3 4 5) 6))

(define (vector-reverse2! v)
  "Reverses in place the vector v"
  (define (swap i j)
    (let ([t (vector-ref v i)])
      (vector-set! v i (vector-ref v j))
      (vector-set! v j t)))
  (do ([i 0 (1+ i)] [j (1- (vector-length v)) (1- j)])
      ([>= i j] v) (swap i j)))

;; (pp (vector-reverse2! (vector 1 2 3 4)))
;; (pp (vector-reverse2! (vector 1 2 3 4 5)))

(define (vector-append2 . v)
  "Appends all vectores from the vector list v"
  (let* ([n (fold (lambda (e b) (+ (vector-length e) b)) 0 v)]
         [a (make-vector n)])
    (let append* ([v v] [i 0])
      (if [null? v] a
          (let ([n (vector-length (car v))])
            (do ([j 0 (1+ j)])
                ([= j n] (append* (cdr v) (+ n i)))
              (vector-set! a (+ i j) (vector-ref (car v) j))))))))

;; (pp (vector-append2 #(1 2 3) #(4 5) #(6 7 8 9)))
