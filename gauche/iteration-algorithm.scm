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
