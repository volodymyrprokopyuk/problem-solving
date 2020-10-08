(define-module
  (laze-algorithm))

(use-modules
 (srfi srfi-27)
 ((ice-9 pretty-print)
  #:select ((pretty-print . pp))))

(random-source-randomize! default-random-source)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DALAYED (LAZY) LIST

(define dl-null '())

(define dl-null? null?)

(define dl-car car)

(define dl-cdr (compose force cdr))

(define-syntax-rule (dl-cons e dl) (cons e (delay dl)))

(define (make-dlist . args)
  "Creates delated list from the arguments args"
  (let dlist* ([l args] [r dl-null])
    (cond
      [(null? l) r]
      [else (dlist* (cdr l) (dl-cons (car l) r))])))

(define (dlist->list dl)
  "Forces the delayed list dl into a list"
  (let list* ([dl dl] [r '()])
    (cond
      [(dl-null? dl) r]
      [else (list* (dl-cdr dl) (cons (dl-car dl) r))])))

#;(let ([dl (make-dlist 1 2 3 4 5)])
  (pp dl)
  (pp (dlist->list dl)))

(define (dl-unfold p f g s)
  "Fundamental constructor of a delayed list"
  (let unfold* ([s (g s)] [r dl-null])
    (cond
      [(p s) r]
      [else (unfold* (g s) (dl-cons (f s) r))])))

#;(let ([dl (dl-unfold (lambda (e) (> e 5)) identity 1+ 0)]
      [rdl (dl-unfold (lambda (i) (> i 10)) (lambda (_) (random-integer 10)) 1+ 0)])
  (pp (dlist->list dl))
  (pp (dlist->list rdl)))

(define (dl-fold f b dl)
  "Fundamental iterator for a delayed list"
  (let fold* ([dl dl] [r b])
    (cond
      [(dl-null? dl) r]
      [else (fold* (dl-cdr dl) (f (dl-car dl) r))])))

#;(let ([s (dl-fold + 0 (make-dlist 1 2 3 4 5))]
      [dl (dl-fold (lambda (e b) (dl-cons e b)) dl-null (make-dlist 1 2 3 4 5))])
  (pp s)
  (pp (dlist->list dl)))

(define (dl-map f dl)
  "Maps the function f over the delayed list dl"
  (let ([rdl (dl-fold (lambda (e b) (dl-cons (f e) b)) dl-null dl)])
    (dl-fold (lambda (e b) (dl-cons e b)) dl-null rdl)))

#;(let ([dl (dl-map 1+ (make-dlist 1 2 3 4 5))])
  (pp (dlist->list dl)))

(define (dl-filter p dl)
  "Filters the delayed list dl with the predicate p"
  (let ([rdl (dl-fold (lambda (e b) (if [p e] (dl-cons e b) b)) dl-null dl)])
    (dl-fold (lambda (e b) (dl-cons e b)) dl-null rdl)))

#;(let ([dl (make-dlist 1 2 3 4 5)])
  (pp (dlist->list (dl-filter odd? dl))))
