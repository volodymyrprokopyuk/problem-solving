(use util.match)

(define st-empty '())

(define st-null? null?)

(define-syntax st-cons
  (syntax-rules ()
    [(_ a d) (cons a (delay d))]))

(define st-car car)

(define (st-cdr s) (force (cdr s)))

(define (st-unfold p f g s :optional (t (constantly st-empty)))
  (let unfold* ([s s])
    (cond
      [(p s) (t s)]
      [else (st-cons (f s) (unfold* (g s)))])))

;; #?=(st-unfold null? car cdr '(a b c d))

(define (st-make . l)
  (st-unfold null? car cdr l))

;; #?=(st-make 'a 'b 'c 'd)

(define list->stream (cut st-unfold null? car cdr <>))

;; #?=(list->stream '(a b c d))

(define (st-unfold-right p f g s :optional (t st-empty))
  (let unfold* ([s s] [r t])
    (cond
      [(p s) r]
      [else (unfold* (g s) (st-cons (f s) r))])))

;; #?=(st-unfold-right null? car cdr '(a b c d))

(define (st-fold f s t)
  (let fold* ([t t] [r s])
    (cond
      [(st-null? t) r]
      [else (fold* (st-cdr t) (f (st-car t) r))])))

;; #?=(st-fold - 0 (list->stream '(1 2)))
;; #?=(st-fold cons '() (list->stream '(a b c d)))

(define (st-fold-right f s t)
  (let fold* ([t t])
    (cond
      [(st-null? t) s]
      [else (f (st-car t) (fold* (st-cdr t)))])))

;; #?=(st-fold-right - 0 (list->stream '(1 2)))
;; #?=(st-fold-right cons '() (list->stream '(a b c d)))

(define stream->list (cut st-fold-right cons '() <>))

;; #?=(stream->list (list->stream '(1 2 3 4 5)))
;; (let* ([s (st-make 'a 'b)]
;;        [t (st-cons (st-car s) (st-cdr s))])
;;   #?=(equal? (stream->list s) (stream->list t)))

(define (st-map f s)
  (let map* ([s s])
    (cond
      [(st-null? s) st-empty]
      [else (st-cons (f (st-car s)) (map* (st-cdr s)))])))

;; #?=(stream->list (st-map (cut + <> 1) (list->stream '(1 2 3 4 5))))

(define (st-map2 f s)
  (st-unfold st-null? (lambda (t) (f (st-car t))) st-cdr s))

;; #?=(stream->list (st-map2 (cut + <> 1) (list->stream '(1 2 3 4 5))))

(define (st-filter p s)
  (let filter* ([s s])
    (cond
      [(st-null? s) st-empty]
      [(p (st-car s)) (st-cons (st-car s) (filter* (st-cdr s)))]
      [else (filter* (st-cdr s))])))

;; #?=(stream->list (st-filter odd? (list->stream '(1 2 3 4 5))))

(define (st-remove p s)
  (st-filter (complement p) s))

;; #?=(stream->list (st-remove odd? (list->stream '(1 2 3 4 5))))

(define (st-take n s)
  (st-unfold
   (match-lambda [(n . s) (or (zero? n) (st-null? s))])
   (match-lambda [(_ . s) (st-car s)])
   (match-lambda [(n . s) (cons (- n 1) (st-cdr s))])
   (cons n s)))

;; #?=(stream->list (st-take 3 (list->stream '(1 2 3 4 5))))
;; #?=(stream->list (st-take 6 (list->stream '(1 2 3 4 5))))

(define (st-from :optional (n 0))
  (st-cons n (st-from (+ n 1))))

;; #?=(stream->list (st-take 7 (st-from)))
;; #?=(stream->list (st-take 7 (st-from 1)))
;; #?=(stream->list (st-take 7 (st-map (cut * <> 10) (st-from))))
;; #?=(stream->list (st-take 7 (st-filter even? (st-from))))
;; #?=(stream->list (st-take 7 (st-remove even? (st-from))))
