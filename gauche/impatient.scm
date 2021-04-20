(use util.match)
(use srfi-42)
(use srfi-13)
(use gauche.lazy)

;; (define-values (a b) (values 1 2))
;; (match-define (a b) '(11 22))
;; (cond
;;   [(< a 0) (set! a (* a 10)) (set! b (* b 10))]
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

;; (define (=abs x) (if [>= x 0] x (- x)))
;; (print (=abs 4) " " (=abs -5))

;; (define (=fact n) (fold-ec 1 (:range i 2 (+ n 1)) i *))
;; (print (map =fact '(0 1 2 3 4 5)))

;; (define (=fact n)
;;   (let fact* ([n n] [r 1])
;;     (if [< n 2] r (fact* (- n 1) (* r n)))))
;; (print (map =fact '(0 1 2 3 4 5)))

;; (define (decorate s :optional (l "[") (r "]")) #"~|l|~|s|~|r|")
;; #?=(decorate "Vlad")
;; #?=(decorate "Vlad" "{" "}")
;; (define (decorate s :key (l "[") (r "]")) #"~|l|~|s|~|r|")
;; #?=(decorate "Vlad")
;; #?=(decorate "Vlad" :r ">" :l "<")
;; #?=(decorate "Vlad" :r "/")

;; (define (=sum . x) (apply + x))
;; (define (=sum :rest x) (fold-ec 0 (:list i x) i +))
;; #?=(=sum 1 2 3 4 5)

;; (define (rsum . x)
;;   (let sum* ([x x] [r 0])
;;     (if [null? x] r (sum* (cdr x) (+ (car x) r)))))
;; #?=(rsum 1 2 3 4 5)

;; (define (box s :key (p 1) (h #\-) (v #\|))
;;   (let* ([n (string-length s)]
;;          [l (make-string (+ n (* 2 (+ p 1))) h)]
;;          [m (make-string p #\space)])
;;     (format #t "~a\n~a~a~a~a~a\n~a\n" l v m s m v l)))

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
