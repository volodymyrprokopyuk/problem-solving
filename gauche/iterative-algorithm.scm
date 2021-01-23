(use srfi-13) ;; string-upcase

(define (make-counter :optional (start 0) (step 1))
  "Returns a counter with start and step"
  (let ([counter start])
    (lambda ()
      (let ([c counter])
        (set! counter (+ c step))
        c))))

;; (let ([counter (make-counter 10 10)])
;;   #?=(counter)
;;   #?=(counter)
;;   #?=(counter))

(define-class <counter> ()
  ([counter :init-value 0 :init-keyword :start]
   [step :init-value 1 :init-keyword :step]
   [forward :allocation :virtual
            :slot-ref (lambda (counter)
                        (let ([c (~ counter 'counter)])
                          (set! (~ counter 'counter) (+ c (~ counter 'step)))
                          c))]))

(define-method forward ([counter <counter>])
  "Forsards the counter one step futher"
  (let ([c (~ counter 'counter)])
    (set! (~ counter 'counter) (+ c (~ counter 'step)))
    c))

;; (let ([counter (make <counter> :start 10 :step 10)])
;;   #?=(forward counter)
;;   #?=(forward counter)
;;   #?=(forward counter)
;;   #?=(~ counter 'forward)
;;   #?=(~ counter 'forward))

(define (proper-list? l)
  "Returns #t if the list l is proper"
  (cond
    [(null? l) #t]
    [(not (pair? l)) #f]
    [else (proper-list? (cdr l))]))

;; #?=(proper-list? '())
;; #?=(proper-list? '(1))
;; #?=(proper-list? '(1 2))
;; #?=(proper-list? '(1 . 2))
;; #?=(proper-list? #f)

(define (cyclic-list? l)
  "Returns #t on a cyclic list using the hare and tortoise algorithm, othewise #f"
  (cond
    [(pair? l)
     (let race* ([t l] [h (cdr l)])
       (cond
         [(eq? h t) #t]
         [(and (pair? h) (pair? (cdr h))) (race* (cdr t) (cddr h))]
         [else #f]))]
    [else #f]))

;; #?=(cyclic-list? '())
;; #?=(cyclic-list? '(1))
;; #?=(cyclic-list? '(1 2))
;; #?=(cyclic-list? '(1 . 2))
;; #?=(cyclic-list? #f)
;; (let ([cl '(1 2 3)])
;;   (set! (cdr cl) cl)
;;   #?=(cyclic-list? cl))

(define (=list? l)
  "Returns #t on a proper list, otherwise #f including a cyclic list"
  (cond
    [(cyclic-list? l) #f]
    [(proper-list? l) #t]
    [else #f]))

;; #?=(=list? '())
;; #?=(=list? '(1))
;; #?=(=list? '(1 2))
;; #?=(=list? '(1 . 2))
;; #?=(=list? #f)
;; (let ([cl '(1 2 3)])
;;   (set! (cdr cl) cl)
;;   #?=(=list? cl))

(define (acronym s :optional (sp #[- ]))
  "Returns an acronym of the words in the string s split by split char set sp"
  #;(apply string
         (map char-upcase
              (map (cut string-ref <> 0)
                   (string-split s sp))))
  #;($ string
     $* map char-upcase
     $ map (cut string-ref <> 0)
     $ string-split s sp)
  ($ string
     $* map (.$ char-upcase (cut string-ref <> 0))
     $ string-split s sp))

;; #?=(acronym "Tail-call optimization")

(define (acronym2 s)
  "Returns an acronym of the words in the string s split by split char set sp"
  ($ string-upcase $ regexp-replace-all #/\b(.)[^- ]*[- ]*/ s "\\1"))

;; #?=(acronym2 "Tail-call optimization")

(define (acronym3 s :optional (sp #[- ]))
  "Returns an acronym of the words in the string s split by split char set sp"
  (with-string-io s
    (lambda ()
      (let ([state 'boundary] [spc (char-set-complement sp)])
        (let read* ([c (read-char)])
          (unless [eof-object? c]
            (cond
              [(and (eq? state 'boundary) (spc c))
               (set! state 'word) ($ write-char $ char-upcase c)]
              [(and (eq? state 'word) (sp c))
               (set! state 'boundary)])
            (read* (read-char))))))))

;; #?=(acronym3 "Tail-call optimization")

(define (pig-language w)
  "Translates the word w into the pig language"
  (let pig* ([l (string->list w)])
    (cond
      [(#[aeiou] (car l)) ($ string $* append l '(#\a #\y))]
      [else (pig* (append (cdr l) ($ list $ car l)))])))

;; #?=(pig-language "elephant")
;; #?=(pig-language "spagetti")

(define (pig-language2 w)
  "Translates the word w into the pig language"
  (regexp-replace #/([^aeiou]*)(.*)/ w "\\2\\1ay"))

;; #?=(pig-language2 "elephant")
;; #?=(pig-language2 "spagetti")

(define (choices ll)
  "Generates all possible choices from a list of lists of elements"
  (cond
    [(null? ll) '(())]
    [else
     (let ([l (car ll)] [cc (choices (cdr ll))])
       (fold (lambda (c s) (append s (map (lambda (e) (cons e c)) l))) '() cc))]))

;; #?=(choices '((a b) (1 2) #;(A B)))

(define (choices2 ll)
  "Generates all possible choices from a list of lists of elements"
  (let choices* ([ll (reverse ll)] [cc '(())])
    (cond
      [(null? ll) cc]
      [else
       (choices* (cdr ll)
                 (fold (lambda (c s) (append s (map (lambda (e) (cons e c)) (car ll))))
                       '() cc))])))

;; #?=(choices2 '((a b) (1 2) #;(A B)))

(define (=combinations n l)
  "Generates all possible combinations of a size n from the set l"
  (cond
    [(zero? n) '(())]
    [(null? l) '()]
    [else (append (map (cut cons (car l) <>) (=combinations (- n 1) (cdr l)))
                  (=combinations n (cdr l)))]))

;; #?=(=combinations 2 '(a b c))

(define (average . l)
  "Returns the average of the argument list l"
  (/ (apply + l) (length l)))

;; #?=(average 1 2)

(define (coefficient-exponent x)
  "Returnn the coefficient and the exponent of the number x in scientific notation"
  (let* ([e (floor (log x 10))] [c (/ x (expt 10 e))])
    (values c e)))

;; #?=(coefficient-exponent 7100)
;; #?=(coefficient-exponent 0.071)

(define (string-justify s :optional (w 88))
  "Justifies the string s to the width w"
  (let ([n (- w (string-length s))] [wl (string-split s " ")])
    (cond
      [(< (length wl) 2) s]
      [else
       (let justify* ([l (drop-right wl 1)] [n n] [r '()])
         (cond
           [(< n 1) (string-join (append (reverse r) l (take-right wl 1)))]
           [(null? l) (justify* (reverse r) n '())]
           [else (justify* (cdr l) (- n 1) (cons (string-append (car l) " ") r))]))])))

;; #?=(string-justify "" 40)
;; #?=(string-justify "Vlad" 0)
;; #?=(string-justify "Vlad" 3)
;; #?=(string-justify "Vlad" 40)
;; #?=(string-justify "Vlad and Lana are leaving together" 40)

(define (justify-io :optional (w 88))
  (let justify* ([s (read-line)])
    (unless (eof-object? s)
      (print (string-justify s w))
      (justify* (read-line)))))

;; (with-input-from-file "iterative-algorithm.scm" justify-io)

(define (=vector-swap! v i j)
  "Swaps in place elements i and j of the vector v"
  (let ([iv (vector-ref v i)])
    (set! (vector-ref v i) (vector-ref v j))
    (set! (vector-ref v j) iv)))

;; #?=(let ([v #(1 2 3 4)]) (=vector-swap! v 1 2) v)

(define (=vector-fold f s v :optional (a #f) (b #f))
  "Folds left the function f over the vector v withing the [a b) starting with the seed s"
  (do ([i (or a 0) (+ i 1)] [s s (f s (vector-ref v i) i)])
      ([>= i (or b (vector-length v))] s)))

;; #?=(=vector-fold (lambda (s e _) (+ s e)) 0 #())
;; #?=(=vector-fold (lambda (s e _) (+ s e)) 0 #(1 2 3 4 5))
;; #?=(=vector-fold (lambda (s e _) (+ s e)) 0 #(1 2 3 4 5) 2 4)
;; #?=(=vector-fold (lambda (s e _) (- s e)) 0 #(1 2 3 4 5))
;; #?=(=vector-fold (lambda (s e i) (if [< (car s) e] s (cons e i)))
;;                  (cons +inf.0 -1) #(1 2 3 4 5))
;; #?=(=vector-fold (lambda (s e i) (if [> (car s) e] s (cons e i)))
;;                  (cons -inf.0 -1) #(1 2 3 4 5) 1 4)

(define (=vector-fold-right f s v :optional (a #f) (b #f))
  "Folds right the function f over the vector v withing the [a b) starting with the seed s"
  (do ([i (- (or b (vector-length v)) 1) (- i 1)] [s s (f (vector-ref v i) s i)])
      ([< i (or a 0)] s)))

;; #?=(=vector-fold-right (lambda (e s _) (+ e s)) 0 #())
;; #?=(=vector-fold-right (lambda (e s _) (+ e s)) 0 #(1 2 3 4 5))
;; #?=(=vector-fold-right (lambda (e s _) (+ e s)) 0 #(1 2 3 4 5) 2 4)
;; #?=(=vector-fold-right (lambda (e s _) (- e s)) 0 #(1 2 3 4 5))
;; #?=(=vector-fold-right (lambda (e s i) (if [< e (car s)] (cons e i) s))
;;                        (cons +inf.0 -1) #(1 2 3 4 5))
;; #?=(=vector-fold-right (lambda (e s i) (if [> e (car s)] (cons e i) s))
;;                        (cons -inf.0 -1) #(1 2 3 4 5) 1 4)

(define (selection-sort! v :optional (c <) (d +inf.0))
  "Returns the sorted in place vector v using selection sort"
  (do ([i 0 (+ i 1)]) ([>= i (- (vector-length v) 1)] v)
    (let ([m (=vector-fold (lambda (s e j) (if [c (car s) e] s (cons e j)))
                           (cons d -1) v i)])
      (=vector-swap! v i (cdr m)))))

;; #?=(selection-sort! #())
;; #?=(selection-sort! #(9 5 3 7 6 0 1 2 8 4 9))
;; #?=(selection-sort! #(9 5 3 7 6 0 1 2 8 4 9) > -inf.0)

(define (kons a b) (lambda (s) (s a b)))
(define (kar p) (p (lambda (a _) a)))
(define (kdr p) (p (lambda (_ b) b)))

;; (let ([p (kons 'a 'b)]) #?=p #?=(kar p) #?=(kdr p))
