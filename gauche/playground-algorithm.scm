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
