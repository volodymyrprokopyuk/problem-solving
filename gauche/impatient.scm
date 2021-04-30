(use util.match)
(use srfi-42)
(use srfi-13)
(use gauche.lazy)
(use gauche.collection)
(use gauche.sequence)
(use srfi-27)
(use data.random)
(use gauche.generator)

(random-source-randomize! default-random-source)
(set! (random-data-seed) (random-integer (expt 2 32)))

;; *** CHAPTER 2

;; (define-values (a b) (values 1 2))
;; (match-define (a b) '(1 2))
;; (cond
;;   [(> a 0) (set! a (* a 10)) (set! b (* b 10))]
;;   [else (set! a 0) (set! b 0)])
;; (print a " " b)

;; (match-define (x x0 y y0) '(1 2 3 4))
;; (define d (let ([dx (- x x0)] [dy (- y y0)]) (sqrt (+ (expt dx 2) (expt dy 2)))))
;; (print d)

;; (display "Vlad")
;; (print "Vlad" "Lana")
;; (format #t "~a ~a\n" "Vlad" "Lana")
;; (let ([name "Vlad"] [age 36])
;;   (display #"~name next year ~(+ age 1)\n")
;;   (format #t "~a next year ~10,2f\n" name (+ age 1)))

;; (let ([name (begin (display "name > ") (flush) (read))]
;;       [age (begin (display "age > ") (flush) (read))])
;;   (display #"~name is ~age\n"))

;; (do ([i 0 (+ i 1)]) ([> i 4]) (display i))
;; (write (list-ec (:string c "Vlad") c))
;; (list-ec (:string c "Vlad") (write c))
;; (display (list-ec (:range i 1 4) (:range j 4 7) (cons i j)))
;; (display (list-ec (:range i 1 4) (:range j 1 4) (not (= i j)) (cons i j)))
;; (display (list-ec (:range i 1 4) (:let k (- 4 i)) (:range j k 4) (cons i j)))

(define (=abs x) (if [>= x 0] x (- x)))
;; (print (=abs 4) " " (=abs -5))

(define (=fact n) (fold-ec 1 (:range i 2 (+ n 1)) i *))
;; (print (map =fact '(0 1 2 3 4 5)))

(define (=fact n)
  (let fact* ([n n] [r 1])
    (if [< n 2] r (fact* (- n 1) (* r n)))))
;; (print (map =fact '(0 1 2 3 4 5)))

(define (decorate s :optional (l "[") (r "]")) #"~|l|~|s|~|r|")
;; #?=(decorate "Vlad")
;; #?=(decorate "Vlad" "{" "}")
(define (decorate s :key (l "[") (r "]")) #"~|l|~|s|~|r|")
;; #?=(decorate "Vlad")
;; #?=(decorate "Vlad" :r ">" :l "<")
;; #?=(decorate "Vlad" :r "/")

(define (=sum . x) (apply + x))
(define (=sum :rest x) (fold-ec 0 (:list i x) i +))
;; #?=(=sum 1 2 3 4 5)

(define (rsum . x)
  (let sum* ([x x] [r 0])
    (if [null? x] r (sum* (cdr x) (+ (car x) r)))))
;; #?=(rsum 1 2 3 4 5)

(define (box s :key (p 1) (h #\-) (v #\|))
  (let* ([n (string-length s)]
         [l (make-string (+ n (* 2 (+ p 1))) h)]
         [m (make-string p #\space)])
    (format #t "~a\n~a~a~a~a~a\n~a\n" l v m s m v l)))

;; (box "Vlad")
;; (box "Vlad" :p 2)
;; (box "Vlad" :p 3 :h #\=)
;; (box "Vlad" :p 4 :h #\# :v #\#)

;; (with-input-from-string "Vlad" (lambda () ($ print $ read)))
;; (print (with-output-to-string (lambda () (display "Lana"))))
;; (print (with-string-io "Vlad" (lambda () ($ display $ string-upcase $ read-line))))

(define (first-car-or-cdr f)
  (with-input-from-file f
    (lambda ()
      (let next* ([cs (generator->lseq read-char)] [i 0])
        (match cs
          [() #f]
          [(#\c (or #\a #\d) #\r . _) i]
          [(c . cs) (next* cs (+ i 1))])))))
;; (print (first-car-or-cdr "impatient.scm"))

;; (let ([f (lazy (with-input-from-file "./bin/run.sh"
;;                  (lambda () ($ display $ port->string $ current-input-port))))])
;;   (force f))

(define-condition-type <app-error> <error> app-error? [reason reason])
;; (guard
;;  (e
;;   [(<app-error> e)
;;    (format #t "ERROR: <app-error> ~a ~a" (reason e) (condition-message e))]
;;   [else (format #t "ERROR: ~a" e)])
;;  (error "Message")
;;  (error <app-error> :reason "Reason" "Message")
;;  (raise (condition [<app-error> (reason "Reason") (message "Message")])))

;; (guard
;;  (e
;;   [(<read-error> e) (format #t "ERROR: read error")]
;;   [else (format #t "ERROR: unknown error")])
;;  (unwind-protect
;;   (begin (error <read-error> 'oh) (print 'ok))
;;   (print 'cleaning-up-1) (print 'leaning-up-2)))

(define (=signum x) (cond [(positive? x) 1] [(negative? x) -1] [else 0]))
;; #?=(map =signum '(4 0 -3 4.1 0.0 -3.2))

;; (do-ec (:range i 10 -1 -1) (format #t "~a " i))
;; (do-ec (:range i 10 -1 -1) (display #"~i "))
;; #?=(fold-ec 1 (:string c "Hello") (char->integer c) *)

;; *** CHAPTER 3

;; (let ([v (make-vector 5)])
;;   (print v) (set! (~ v 0) 10) (print v))
;; (let ([l '()])
;;   (set! l (cons 1 l)) (print l) (set! l (append l '(2 3 4))) (print l))
;; (let ([v (vector 1 2 3 4)])
;;   (do-ec (:vector e v) (display #"~e "))
;;   (do-ec (:range i 0 8 2) (display #"~i "))
;;   (do-ec (:range i 7 -1 -1) (display #"~i "))
;;   (print (vector-ec (:vector e v) (* e 10)))
;;   (print (vector-ec (:vector e v) (if [even? e]) (* e 10)))
;;   (print (list->vector (map (cut * <> 10) (filter even? (vector->list v)))))
;;   (print ($ list->vector $ map (cut * <> 10) $ filter even? $ vector->list v))
;;   (print (($ list->vector $ map (cut * <> 10) $ filter even? $ vector->list $) v))
;;   (print ($ map-to <vector> (cut * <> 10) $ filter even? v)))

;; (let ([v (vector 0 1 -2 3 -4 5)])
;;   (print (vector-ec (:vector e v) (if [>= e 0]) e))
;;   (print (remove-to <vector> negative? v))
;;   (print (fold-ec 0 (:vector e v) e +))
;;   (print (fold + 0 v))
;;   (print (fold-ec +inf.0 (:vector e v) e min))
;;   (print (fold min +inf.0 v))
;;   (print (sort v))
;;   (print (sort v >))
;;   (print (fold-ec 0 (:vector _ v) 1 +))
;;   (print (fold (lambda (_ s) (+ s 1)) 0 v)))

;; (let* ([t (make-vector 5)] [n (vector-length t)])
;;   (do ([i 0 (+ i 1)]) ([>= i n]) (set! (~ t i) (make-vector (+ i 1) 0)))
;;   (for-each-with-index (lambda (i _) (set! (~ t i) (make-vector (+ i 1) 0))) t)
;;   (do-ec (:vector _ (index i) t) (set! (~ t i) (make-vector (+ i 1) 0)))
;;   (print t))

;; (let* ([g1 (integers$ 5)]
;;        [g2 (samples$ '(head tail))]
;;        [g3 (regular-strings$ #/\w{5}/)]
;;        [g4 (samples-from (list g1 g2 g3))])
;;   (print (generator->list g1 7))
;;   (print (generator->list g2 7))
;;   (print (generator->list g3 7))
;;   (print (generator->list g4 14)))

;; (let ([v (vector-tabulate 5 (lambda (_) (random-integer 5)))]) (print v))
;; (let* ([r (integers$ 5)] [v (vector-tabulate 5 (lambda (_) (r)))]) (print v))

;; (let* ([v (vector 1 2 3 4 5)] [n (vector-length v)])
;;   (do-ec (:parallel (:range i 0 n 2) (:range j 1 n 2))
;;             (set! (subseq v i) (list (~ v j) (~ v i))))
;;   (print v))

(define (reverse-every n l)
  (let swap* ([l l] [g '()] [r '()])
    (cond
      [(null? l) ($ append $* reverse $ cons g r)]
      [(< (length g) (- n 1)) (swap* (cdr l) (cons (car l) g) r)]
      [else (swap* (cdr l) '() (cons (cons (car l) g) r))])))

(define (reverse-every n l)
  (receive (g r)
      (fold2
       (lambda (e g r)
         (if [< (length g) n] (values (cons e g) r) (values (list e) (cons g r))))
       '() '() l)
    ($ append $* reverse $ cons g r)))

;; #?=(reverse-every 3 '(1 2 3 4 5 6 7 8))

;; (let* ([v (vector 0 -1 2 -3 4 -5)]
;;        [p (vector-ec (:vector e v) (if [> e 0]) e)]
;;        [n (vector-ec (:vector e v) (if [<= e 0]) e)]
;;        [r (vector-ec (:vector e p n) e)])
;;   (print r))

;; *** CHAPTER 4

;; (let ([h (hash-table-r7 eq-comparator 'alice 7 'bob 5 'cindy 8)]
;;       [m (alist->tree-map '((fred . 7) (alice . 7) (bob . 5) (cindy . 8))
;;                           default-comparator)])
;;   (print (hash-table->alist h))
;;   (print (~ h 'bob))
;;   (print (hash-table-get h 'bobx -1))
;;   (hash-table-put! h 'fred 7)
;;   (print (hash-table->alist h))
;;   (hash-table-delete! h 'fred)
;;   (print (hash-table->alist h))
;;   (for-each (match-lambda [(k . v) (hash-table-put! h k v)]) '((vlad . 9) (lana . 10)))
;;   (print (hash-table->alist h))
;;   (hash-table-for-each h (cut format #t "~a -> ~a " <> <>))
;;   (print (hash-table-keys h) " " (hash-table-values  h))
;;   (tree-map-put! m 'vlad 9)
;;   (tree-map-put! m 'lana 10)
;;   (print (tree-map->alist m)))

;; (match-let ([(_ b _) '(1 vlad #t)]) (print b))
;; (let-values ([(_ b _) (values 1 'vlad #t)]) (print b))

;; (print (list-ec (:parallel (:list k '(1 2 3)) (:list v '(#\a #\b #\c))) (list k v)))
;; (print (map list '(1 2 3) '(#\a #\b #\c)))
;; (do-ec (:list e '((1 a) (2 b) (3 c)))
;;        (match-let ([(k v) e]) (format #t "~a -> ~a " k v)))

(let ([h (hash-table-r7 eq-comparator 'a 1 'b 3 'c 7)]
      [h2 (make-hash-table eq-comparator)])
  (hash-table-for-each h (lambda (k v) (hash-table-put! h k (* v (- 1 0.1)))))
  (print (hash-table->alist h))
  (do-ec (:list p (hash-table->alist h))
         (match-let([(k . v) p]) (hash-table-put! h2 k v)))
  (print (hash-table->alist h2)))
