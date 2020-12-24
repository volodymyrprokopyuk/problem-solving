(define (repeat f n)
  "Composes the function f n times"
  (cond
    [(zero? n) identity]
    [(= n 1) f]
    [else (lambda (x) (f ((repeat f (- n 1)) x)))]))

;; #?=((repeat cdr 3) '(a b c d e f))
;; #?=((repeat (cut + <> 1) 4) 0)
;; #?=((repeat cdr 0) '(a b c d e f))
;; #?=((repeat (cut + <> 1) 0) 0)

;; #?=(map identity '(a b c d))
;; #?=(filter (lambda _ #t) '(a b c d))
;; #?=(fold (lambda (e s) (append s (list e))) '() '(a b c d))
;; #?=(fold-right cons '() '(a b c d))

(define (=map f l)
  (fold-right (lambda (e s) (cons (f e) s)) '() l))

;; #?=(=map identity '(a b c d))

(define (=filter p l)
  (fold-right (lambda (e s) (if [p e] (cons e s) s)) '() l))

;; #?=(=filter (lambda _ #t) '(a b c d))

(define (downup l)
  "Returns a list of sublists of the list l by dopping down and then by adding up"
  (cond
    [(null? l) '()]
    [else
     (let ([d
            (let downup* ([l (reverse l)] [r '()])
              (cond
                [(null? l) (reverse r)]
                [else (downup* (cdr l) (cons (reverse l) r))]))])
       (append d ($ cdr $ reverse d)))]))

;; #?=(downup '(a b c d))
;; #?=(downup '())

(define (downup2 l)
  "Returns a list of sublists of the list l by dopping down and then by adding up"
  (cond
    [(null? l) '()]
    [(= (length l) 1) (list l)]
    [else (append (list l) (downup2 (drop-right l 1)) (list l))]))

;; #?=(downup2 '(a b c d))
;; #?=(downup2 '())

(define (pig s)
  "Translates a word into the pig language"
  (let ([l (string->list s)]
        [c #[^AEIOUaeiou]]
        [ay (.$ list->string (cut append <> (list #\a #\y)))])
    (cond
      [(zero? (string-length s)) s]
      [(every c l) (ay l)]
      [else
       (let pig* ([l l])
         (cond
           [(c (car l)) (pig* (append (cdr l) (list (car l))))]
           [else (ay l)]))])))

;; #?=(pig "")
;; #?=(pig "Tss")
;; #?=(pig "Alex")
;; #?=(pig "Vlad")

(define (explode l)
  "Returns a list of one element lists from the list l"
  (let explode* ([l (reverse l)] [r '()])
    (cond
      [(null? l) r]
      [else (explode* (cdr l) (cons (list (car l)) r))])))

;; #?=(explode '())
;; #?=(explode '(a b c d))

(define (overlapped-pairs l)
  "Returns a list of overlapped pairs from the list l"
  (cond
    [(< (length l) 3) (list l)]
    [else
     (let overlap* ([l (cdr l)] [a (car l)] [r '()])
       (cond
         [(null? l) (reverse r)]
         [else (overlap* (cdr l) (car l) (cons (list a (car l)) r))]))]))

;; #?=(overlapped-pairs '(a b))
;; #?=(overlapped-pairs '(a b c d))
