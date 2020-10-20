(define-module
  (vector-algorithm)
  #:export (successive-powers vector-linear-search))

(use-modules
 (ice-9 curried-definitions)
 (srfi srfi-1) ;; List library
 (srfi srfi-42) ;; Comprehensions
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MATRIX ALGEBRA

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data abstraction primitives

(define ((mx-gen gen) nr nc)
  "Creates a row major matrix of dimension nr x nc using the generator gen"
  (let* ([n (* nr nc)] [m (make-vector (1+ n))])
    (vector-set! m n nc)
    (do ([k 0 (1+ k)]) ([= k n] m)
      (let ([i (quotient k nc)] [j (remainder k nc)])
        (vector-set! m k (gen i j))))))

;; (pp ((mx-gen (const 0)) 3 4))
;; (pp ((mx-gen (lambda (ri ci) (cons ri ci))) 3 4))

(define ((mx-ref m) i j)
  "Returns a matrix element identified by the row and column indices i j"
  (let ([nr (mx-nrow m)] [nc (mx-ncol m)])
    (when [>= i nr] (error "mx-ref: row index out of bound:" i))
    (when [>= j nc] (error "mx-ref: column index out of bound:" j))
    (vector-ref m (+ (* i (mx-ncol m)) j))))

;; (let* ([m ((mx-gen (lambda (ri ci) (cons ri ci))) 3 4)]
;;        [mr (mx-ref m)])
;;   (pp (mr 0 0))
;;   (pp (mr 1 2))
;;   (pp (mr 2 1)))

(define (mx-nrow m)
  "Returns the number of rows in the matrix m"
  (quotient (1- (vector-length m)) (mx-ncol m)))

;; (pp (mx-nrow ((mx-gen (const 0)) 3 4)))

(define (mx-ncol m)
  "Returns the number of columns in the matrix m"
  (vector-ref m (1- (vector-length m))))

;; (pp (mx-ncol ((mx-gen (const 0)) 3 4)))

(define ((mx-set! m) i j e)
  "Sets the element of the matrix m identified by the indices i and j to e"
  (let ([nr (mx-nrow m)] [nc (mx-ncol m)])
    (when [>= i nr] (error "mx-set!: row index out of bound:" i))
    (when [>= j nc] (error "mx-set!: column index out of bound:" j))
    (vector-set! m (+ (* i nc) j) e)))

;; (let ([m ((mx-gen (const 0)) 3 4)])
;;   ((mx-set! m) 1 2 10)
;;   (display (mx-format m)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data abstraction algorithms

(define (vector->matrix v nc)
  "Creates a matrix from the vector v in row major order and the number of columns nc"
  (when [not (zero? (remainder (vector-length v) nc))]
    (error "vector->matrix: invalid number of elements"))
  (let ([nr (quotient (vector-length v) nc)])
    ((mx-gen (lambda (i j) (vector-ref v (+ (* i nc) j)))) nr nc)))

;; (let ([m (vector->matrix #(1 2 3 4 5 6) 3)])
;;   (display (mx-format m)))

(define (mx-format m)
  "Formats the matrix m in the row major order"
  (define (format*)
    (let ([nr (mx-nrow m)]
          [nc (mx-ncol m)]
          [mr (mx-ref m)])
      (list-ec (:range i nr) (:range j nc)
               (format #f (if [= j (1- nc)] "~s\n" "~s ") (mr i j)))))
  (string-join (format*) ""))

;; (display (mx-format ((mx-gen (lambda (ri ci) (cons ri ci))) 3 4)))

(define (mx-row m i)
  "Returns the row identified by the index i of the matrix m"
  (let* ([mr (mx-ref m)] [nc (mx-ncol m)] [v (make-vector nc)])
    (do ([j 0 (1+ j)]) ([= j nc] v) (vector-set! v j (mr i j)))))

;; (let* ([m ((mx-gen (lambda (ri ci) (cons ri ci))) 3 4)])
;;   (pp (mx-row m 0))
;;   (pp (mx-row m 2)))

(define (mx-col m j)
  "Returns the column identified by the index j of the matrix m"
  (let* ([mr (mx-ref m)] [nr (mx-nrow m)] [v (make-vector nr)])
    (do ([i 0 (1+ i)]) ([= i nr] v) (vector-set! v i (mr i j)))))

;; (let* ([m ((mx-gen (lambda (ri ci) (cons ri ci))) 3 4)])
;;   (pp (mx-col m 0))
;;   (pp (mx-col m 3)))

(define (mx-transpose m)
  "Transposes the matrix m"
  (let ([mr (mx-ref m)] [nr (mx-nrow m)] [nc (mx-ncol m)])
    ((mx-gen (lambda (i j) (mr j i))) nc nr)))

;; (let* ([m ((mx-gen (lambda (ri ci) (cons ri ci))) 3 4)]
;;        [mt (mx-transpose m)])
;;   (display (mx-format mt)))

(define (mx-dot-product x y)
  "Returns the dot product of the compatible matrices x and y"
  (let ([xnr (mx-nrow x)]
        [xnc (mx-ncol x)]
        [ynr (mx-nrow y)]
        [ync (mx-ncol y)]
        [xr (mx-ref x)]
        [yr (mx-ref y)])
    (when [not (= xnc ynr)] (error "mx-dot-product: not compatible matrices"))
    ((mx-gen
      (lambda (i j)
        (do ([k 0 (1+ k)] [e 0 (+ (* (xr i k) (yr k j)) e)])
            ([= k xnc] e))))
     xnr ync)))

;; (let* ([x (vector->matrix #(1 2 3 4 5 6) 3)]
;;        [y (vector->matrix #(7 8 9 10 11 12) 2)]
;;        [m (mx-dot-product x y)])
;;   (display (mx-format m)))

(define (mx+ x y)
  "Adds two matrices x and y"
  (let ([xnr (mx-nrow x)]
        [xnc (mx-ncol x)]
        [ynr (mx-nrow y)]
        [ync (mx-ncol y)]
        [xr (mx-ref x)]
        [yr (mx-ref y)])
    (when [not (= xnr ynr)] (error "mx+: not compatible matrices rows"))
    (when [not (= xnc ync)] (error "mx+: not compatible matrices columns"))
    ((mx-gen (lambda (i j) (+ (xr i j) (yr i j)))) xnr xnc)))

;; (let* ([x (vector->matrix #(1 2 3 4 5 6) 3)]
;;        [y (vector->matrix #(7 8 9 10 11 12) 3)]
;;        [m (mx+ x y)])
;;   (display (mx-format m)))

(define (mx-scalar-product m c)
  "Multiplies the matrix m by the scalr c"
  (let ([nr (mx-nrow m)] [nc (mx-ncol m)] [mr (mx-ref m)])
    ((mx-gen (lambda (i j) (* (mr i j) c))) nr nc)))

;; (let* ([m (vector->matrix #(1 2 3 4 5 6) 3)]
;;        [cm (mx-scalar-product m 10)])
;;   (display (mx-format cm)))
