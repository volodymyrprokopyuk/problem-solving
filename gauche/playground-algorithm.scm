;; #?=(+ 1 2)
;; #?,(+ 1 2)

;; #?=(values 1 2 3)
;; #?,((lambda (a) (values a 1 2 3)) 0)

#|
(define |vlad and lana| "Vlad and lana")
(pp |vlad and lana|)
|#

(define (f a b :optional [c 'C] d)
  (values a b c d))

;; #?,(f 'a 'b)
;; #?,(f 'a 'b 'c 'd)

(define (g a b :key [c 'C] d)
  (values a b c d))

;; #?,(g 'a 'b)
;; #?,(g 'a 'b :d 'd :c 'c)

(define (h a b :rest r)
  (values a b r))

;; #?,(h 'a 'b)
;; #?,(h 'a 'b 'c 'd)

;; #?=(map (cut * <> 10) '(1 2 3 4 5))
;; #?,((cut + <> <>) 1 2)
;; #?,((cut + <> <...>) 1 2 3)
;; #?,((cute + (+ 1 2) <>) 3)

;; #?=(filter (cut odd? <>) (map (cut * <> 3) '(1 2 3 4 5)))
;; #?=($ filter (cut odd? <>) $ map (cut * <> 3) '(1 2 3 4 5))

(define case-identity
  (case-lambda
    [() 'zero]
    [(a) `(one ,a)]
    [(a b) `(two ,a ,b)]
    [(a b . r) `(many ,a ,b ,@r)]))

;; #?,(case-identity)
;; #?,(case-identity 'a)
;; #?,(case-identity 'a 'b)
;; #?,(case-identity 'a 'b 'c 'd)

;; #?=(let ([l (list 1 2)])
;;      (set! (car l) 10)
;;      (set! (cadr l) 20)
;;      (set! (car (cdr l)) 200)
;;      l)

;; #?=(let ([l '(1)] [v (vector (list 1 2 3))] [i 0] [p (cons 10 20)])
;;      (push! l 2) (push! (cdr l) 10) (print l)
;;      (pop! l) (pop! l) (print l)
;;      (push! (vector-ref v 0) 0) (print v)
;;      (pop! (vector-ref v 0)) (print v)
;;      (inc! i) (inc! i 10) (dec! i) (dec! i 10)
;;      (update! (car p) (cut + <> 1)) (update! (cdr p) (cut + <> 2)) p)

;; #?=(cond
;;      [(assq 'b '((a . 1) (b . 2))) pair? => cdr]
;;      [else #f])

;; #?=(case 3
;;      [(1 2) => (cut + <> 10)]
;;      [else => (cut + <> 100)])

;; #?=(and-let* ([al '((a . 1) (b . 2))]
;;               [p (assq 'a al)]
;;               [e (cdr p)]
;;               [(number? e)])
;;              (+ e 1))

;; (let ([l (do ([i 0 (+ i 1)] [l '() (cons (lambda () i) l)]) ([= i 5] (reverse l)))])
;;   (for-each (lambda (c) (print (c))) l))

;; #?=(let ([x 1] [y 2] [z '(3 4)])
;;      ;; (reverse `(a b ,x ,y ,@z))
;;      (identity `#(a b ,x ,y ,@z)))

(define (=even? x)
  (if [zero? x] #t (=odd? (- x 1))))

(define (=odd? x)
  (if [zero? x] #f (=even? (- x 1))))

;; #?,(=even? 3)
;; #?,(=odd? 3)

;; #?=(letrec* ([x (+ y 1)] [y (+ x 1)])
;;      (cons x y))

;; (define-values (lo hi) (min&max 1 5 -4 23))
;; #?=(cons lo hi)
;; (define-values qr (quotient&remainder 23 5))
;; #?=qr

;; (sys-exec "ls" '("ls" "-lah") :directory "/home/vlad/Projects")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (define (ratio n d :optional (p 2))
;;   ($ (cut / <> (expt 10 p)) $ round $ (cut * <> (expt 10 p)) $ exact->inexact $ / n d))

;; #?=(ratio 2 3 3)

(define-syntax =append
  (syntax-rules ()
    [(_ (a ...) ...) (list a ... ...) #;`(a ... ...)]))

;; (trace-macro '=append)
;; #?=(=append (1 2 3) (4) (5 6))
;; #?=(macroexpand '(=append (1 2 3) (4) (5 6)))

(define-syntax =m1
  (syntax-rules ()
    [(_ a b) (cons a b)]
    [(_ a b c) (list a b c)]
    [(_ . ?) (syntax-error "malformed =m1" (=m1 . ?))]))

;; #?=(=m1 'a 'b 'c)

;; (define v)
;; #?=v
;; #?=((lambda (:optional x) x) )
;; #?=(if #f #f)
;; #?=(undefined)

;; (let* ([l (cons 'b (cons 'a ()))]
;;        [dl (cons 'c (cons 'b 'a))]
;;        [p (cons 'a ())]
;;        [cl (cons 'b p)])
;;   #?=l
;;   #?=(list? l)
;;   #?=dl
;;   #?=(dotted-list? dl)
;;   (set! (cdr p) cl)
;;   #?=cl
;;   #?=(circular-list? cl))

;; (let ([al (acons 'b 2 (acons 'a 1 '()))])
;;   #?=(assoc 'b al)
;;   (set! al (assoc-set! al 'b 20))
;;   #?=(assoc 'b al)
;;   (set! al (assoc-set! al 'c 3))
;;   #?=(assoc 'c al))

;; #?=(get-keyword :B '(:a 1 :b 2) #f)

;; (let ([cs #[a-z]]) #?=(cs #\a) #?=(cs #\A))

;; (let* ([ms "Multi-line \
;;         very long \
;;         string"]
;;        [gv #"~ms with Gauche version ~(gauche-version)"]
;;        [ds #"Date: ~(sys-strftime \"%Y-%m-%d\" (sys-localtime (sys-time)))"]
;;        [ds2 #"Date: ~($ sys-strftime \"%Y-%m-%d\" $ sys-localtime $ sys-time)"]
;;        [n 7] [ss #"Scheme standard: R~|n|RS"])
;;   #?=ms #?=gv #?=ds #?=ds2 #?=ss)

;; (and-let* ([rx (string->regexp "^(Value:) +(.+)$" )]
;;            [rm (rxmatch rx "Value: ok")])
;;   #?=(rxmatch-substring rm) #?=(rxmatch-substrings rm))

;; (and-let* ([rm1 (#/(\w+)/ "Regular expression in Gauche")]
;;            [rm2 (#/(?<first>\w+)/ "Regular expression in Gauche")]
;;            [rm3 (#/(\d+)_\1/ "12_12")]
;;            [rm4 (#/(?<word>[-\w]+)/ "regular-expression")]
;;            [rm5 (#/(\w+) (?=positive lookahead)/ "ok positive lookahead")]
;;            [rm6 (#/(?<=positive lookbehind) (\w+)/ "positive lookbehind ok")])
;;   #?=(rm1 1) #?=(rm2 'first) #?=(rm3 0) #?=(rm4 'word) #?=(rm5 1) #?=(rm6 1))

;; #?=(regexp-replace-all #/(?<vowel>[aeiou])/ "Vlad and Lana" "|\\k<vowel>|")
;; #?=(regexp-replace-all #/(?<vowel>[aeiou])/ "Vlad and Lana" (lambda (rm) #"|~(rm 'vowel)|"))

;; #?=(vector-tabulate 10 square)

;; Should be part of Gauche's core library
(define (const v)
  "Returns a procedure that accepts any number of arguments and always returns the value v"
  (lambda _ v))

;; (let ([ht (hash-table eq-comparator '(a . 1) '(b . 2))])
;;   ;; Workaround that implements const with cut and identity
;;   #?=(hash-table-ref ht 'b (cut identity #f) identity)
;;   ;; More readable, consice, and idiomatic equivalent
;;   #?=(hash-table-ref ht 'b (const #f) identity))

;; (let ([ht (make-hash-table eq-comparator)]
;;       [ht2 (hash-table eq-comparator '(a . 10) '(b . 20))]
;;       [ht3 (hash-table-unfold null? (lambda (s) (values (caar s) (cdar s))) cdr
;;                               '((a . 100) (b . 200)) eq-comparator )]
;;       [ht4 (make-hash-table eq-comparator)]
;;       [ht5 (alist->hash-table '((a . 1) (b . 2)))])
;;   (hash-table-put! ht 'a 1)
;;   (hash-table-put! ht 'b 2)
;;   (hash-table-put! ht 'b 2)
;;   #?=(hash-table-get ht 'b)
;;   #?=(hash-table-get ht2 'b)
;;   #?=(hash-table-get ht3 'b)
;;   #?=(hash-table-ref ht 'b (cut identity #f) identity)
;;   (hash-table-set! ht2 'a 11 'b 22 'c 33)
;;   #?=(hash-table-get ht2 'a)
;;   #?=(hash-table-get ht2 'c)
;;   #?=(hash-table-push! ht4 'a 1)
;;   #?=(hash-table-push! ht4 'a 2)
;;   #?=(hash-table-push! ht4 'b 10)
;;   #?=(hash-table-push! ht4 'b 20)
;;   #?=(hash-table-get ht4 'a)
;;   #?=(hash-table-get ht4 'b)
;;   #?=(hash-table-pop! ht4 'a)
;;   #?=(hash-table-update! ht4 'a (cut cons 3 <>))
;;   #?=(hash-table-get ht4 'a)
;;   #?=(hash-table-get ht5 'b)
;;   (set! (ref ht5 'a) 10)
;;   #?=(ref ht5 'a)
;;   (hash-table-for-each ht5 print)
;;   #?=(hash-table-map ht5 cons)
;;   #?=(hash-table-fold ht5 (lambda (_ v s) (+ v s)) 0)
;;   #?=(hash-table-find ht5 (lambda (_ v) (and (= v 10) v))))

;; #?=(~ '(a b c) 1)
;; #?=(~ '#(a b c) 2)
;; #?=(~ "abc" 0)
;; #?=(~ (hash-table eq-comparator '(a . 1) '(b . 2)) 'b)
;; #?=(~ ($ sys-localtime $ sys-time) 'hour)
;; #?=(~ '((a b) (c d)) 1 1)
;; (let ([l '((a b) (c d))])
;;   (set! (~ l 1 1) 'D)
;;   #?=l)

;; #?=(apply + (map * '(1 2 3) '(4 5 6)))
;; #?=((.$ (cut apply + <>) (cut map * <...>)) '(1 2 3) '(4 5 6))
;; #?=($ (cut apply + <>) $ (cut map * <...>) '(1 2 3) '(4 5 6))

(define-method object-apply ([s <string>] [i <integer>])
  (string-ref s i))

;; #?=("abc" 2)

;; (dynamic-wind
;;   (cut print 'set-up)
;;   ;; (cut error "oh")
;;   (cut print 'body)
;;   (cut print 'restore))

;; #?=(values->list (values 1 2))
;; #?=(receive (a b) (values 1 2) (cons a b))
;; #?=(let-values ([(a b) (values 1 2)]) (cons a b))

;; #?=(with-input-from-string "Vlad and Lana\nScheme and Gauche"
;;      (cut generator-fold cons '() read))

;; #?=(with-input-from-string "Vlad and Lana\nScheme and Gauche"
;;      (cut generator-map symbol->string read))

;; #?=(with-input-from-string "Vlad and Lana\nScheme and Gauche"
;;      (cut generator-find #/and/ read-line))

;; #?=(guard (e [(<read-error> e) (format #t "ERROR: read-error") 'read-error]
;;              [else 'other-error])
;;      (print 'ok) (error 'oh) (read-from-string "(abc"))

;; #?=(guard (e [(integer? e) 'integer-error])
;;      (raise 1))

;; #?=(guard (e [(<error> e) 'error])
;;      (raise (error "oh"))
;;      (raise (condition (<error> (message "oh2")))))

(define-condition-type <app-error> <error>
  app-error?
  (reason app-error-reason))

;; #?=(guard (e [(<app-error> e) (app-error-reason e)])
;;           (error <app-error> :reason 'app-error-reason)
;;           (raise (condition (<app-error> (reason 'app-error-reason2)))))

;; (dynamic-wind (cut print 'pre) (cut print 'body) (cut print 'post))

;; (unwind-protect
;;  (begin (print 'pre) (print 'ok) (error 'oh))
;;  (print 'post) (print 'cleanup))

;; (guard (e [else (report-error e)])
;;           (error "oh error"))

(use gauche.parameter)

;; #?=(let ([os (open-output-string)])
;;      (parameterize ([current-output-port os])
;;        (display 'ok))
;;      (get-output-string os))

;; (write '(1 10 100 1000) (make-write-controls :base 16 :radix #t))
;; (write (iota 100) (make-write-controls :pretty #t :width 20))
;; (let* ([x (list 1 2 3)] [y (list x x)])
;;   (set! (cdddr x) x)
;;   (write x) (write y)
;;   (newline)
;;   (display x) (display y)
;;   (newline)
;;   (print x y))
;; #?=(format "~a ~s" "ok" "ok")

;; #?=(sort '(("abc" "xwz") ("def" "klm")) string<? (cut ~ <> 1))

(define-class <point> ()
  ([x :init-value 0.0 :init-keyword :x :accessor point-x]
   [y :init-value 0.0 :init-keyword :y :accessor point-y]
   [xy :allocation :virtual :getter point-xy
       :slot-ref (lambda (p) (cons (~ p 'x) (~ p 'y)))]))

(define-method move-point! ([p <point>] dx dy)
  "MOve object by dx and dy"
  (inc! (point-x p) dx) (inc! (point-y p) dy))

(define-method write-object ([p <point>] port)
  "Write <point> object"
  (format port "#<<point> [~a, ~a]>" (~ p 'x) (~ p 'y)))

;; (let ([p (make <point> :x 1.0 :y 2.0)])
;;   (print (point-x p) " " (point-y p))
;;   (slot-set! p 'x 10.0)
;;   (print (slot-ref p 'x))
;;   (set! (~ p 'y) 20.0)
;;   (print (~ p 'y))
;;   (set! (point-x p) 100.0)
;;   (print (point-x p))
;;   (move-point! p 0.0 180.0)
;;   (print (~ p 'x) " " (~ p 'y))
;;   (display p)
;;   (print (point-xy p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use util.match)

;; (define-values (a b) (values 1 2))
;; (if [> a 0]
;;     (begin (set! a (* a 10)) (set! b (* b 10)))
;;     (begin (set! a 0) (set! b 0)))

(match-define (a b) '(11 22))
(cond
  [(< a 0) (set! a (* a 10)) (set! b (* b 10))]
  [else (set! a 0) (set! b 0)])

(print a " " b)
