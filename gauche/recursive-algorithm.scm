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

;; #?=(downup '())
;; #?=(downup '(a b c d))

(define (downup2 l)
  "Returns a list of sublists of the list l by dopping down and then by adding up"
  (cond
    [(null? l) '()]
    [(= (length l) 1) (list l)]
    [else (append (list l) (downup2 (drop-right l 1)) (list l))]))

;; #?=(downup2 '())
;; #?=(downup2 '(a b c d))

(define (down l)
  "Returns a list of shrinking sublists of the list l"
  (cond
    [(null? l) '()]
    [else (cons l (down (drop-right l 1)))]))

;; #?=(down '())
;; #?=(down '(a b c d))

(define (up l)
  "Returns a list of growing sublists of the list l"
  (cond
    [(null? l) '()]
    [else (append (up (drop-right l 1)) (list l))]))

;; #?=(up '())
;; #?=(up '(a b c d))

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
  (let overlap* ([l l] [r '()])
    (cond
      [(< (length l) 2) (reverse r)]
      [else (overlap* (cdr l) (cons (take l 2) r))])))

;; #?=(overlapped-pairs '(a b))
;; #?=(overlapped-pairs '(a b c d))

(define (non-overlapped-pairs l)
  "Returns a list of non-overlapped pairs of the list l"
  (let non-overlap* ([l l] [r '()])
    (cond
      [(< (length l) 3) ($ reverse $ cons l r)]
      [else (non-overlap* (cddr l) (cons (take l 2) r))])))

;; #?=(non-overlapped-pairs '(a b))
;; #?=(non-overlapped-pairs '(a b c d e))

(define (=reverse l)
  "Returns a reversed list of the list l"
  (cond
    [(null? l) l]
    #;[else (append (=reverse (cdr l)) (list (car l)))]
    [else (cons (last l) (=reverse (drop-right l 1)))]))

;; #?=(=reverse '(a b c d))

(define (evens l)
  "Returns the elements at even positions from the list l"
  (let evens* ([l l] [i 1] [r '()])
    (cond
      [(null? l) (reverse r)]
      [(even? i) (evens* (cdr l) (+ i 1) (cons (car l) r))]
      [else (evens* (cdr l) (+ i 1) r)])))

;; #?=(evens '())
;; #?=(evens '(1 2 3 4 5 6 7))

(define (evens2 l)
  "Returns the elements at even positions from the list l"
  (let evens* ([l l] [r '()])
    (cond
      [(< (length l) 2) (reverse r)]
      [else (evens* (cddr l) (cons (cadr l) r))])))

;; #?=(evens2 '())
;; #?=(evens2 '(1 2 3 4 5 6 7))

(define (filter-doubles s)
  "Returns a list of double letter strings from the string s"
  (let filter* ([l (string->list s)] [r '()])
    (cond
      [(< (length l) 2) (reverse r)]
      [(every (cut char=? <> (car l)) (take l 2))
       (filter* (cdr l) (cons ($ list->string $ take l 2) r))]
      [else (filter* (cdr l) r)])))

;; #?=(filter-doubles "bookkeeper")
;; #?=(filter-doubles "mississippi")

(define (every-nth l n)
  "Returns a list of every nth element of the list l"
  (let every-nth* ([l l] [r '()])
    (cond
      [(null? l) (reverse r)]
      [(< (length l) n) ($ reverse $ cons (car l) r)]
      [else (every-nth* (drop l n) (cons (car l) r))])))

;; #?=(every-nth '(1 2 3 4 5 6 7 8 9) 3)

(define (all-pairs l)
  "Returns a list of all possible pairs of elements from the list l"
  (let first* ([m l] [r '()])
    (cond
      [(null? m) (reverse r)]
      [else
       (let second* ([n l] [s r])
         (cond
           [(null? n) (first* (cdr m) s)]
           [else (second* (cdr n) (cons (list (car m) (car n)) s))]))])))

;; #?=(all-pairs '(a b c))

(define (all-pairs2 l)
  "Returns a list of all possible pairs of elements from the list l"
  (append-map (lambda (e) (map (lambda (f) (list e f)) l)) l))

;; #?=(all-pairs2 '(a b c))

(define (all-tuples l n)
  "Returns a list of all n-tules of elements from the list l"
  (cond
    [(zero? n) '(())]
    [else
     (append-map
      (lambda (e) (map (lambda (f) (cons e f)) (all-tuples l (- n 1)))) l)])
  )

;; (pprint (all-tuples '(a b c) 3))

(define (remove1 x l :optional (c equal?))
  "Removes the first occurence of the element x in the list l"
  (let remove* ([l l] [r '()])
    (cond
      [(null? l) (reverse r)]
      [(c (car l) x) (append (reverse r) (cdr l))]
      [else (remove* (cdr l) (cons (car l) r))])))

;; #?=(remove1 3 '(1 2 3 4 5 3) eqv?)
;; #?=(remove1 9 '(1 2 3 4 5 3) eqv?)

(define (selection-sort l :optional (c <))
  "Returns a sorted copy of the list l"
  (let sort* ([l l] [r '()])
    (cond
      [(null? l) (reverse r)]
      [else
       (let* ([m (reduce (lambda (e s) (if [c e s] e s)) #f l)]
              [k (remove1 m l eqv?)])
         (sort* k (cons m r)))])))

#?=(selection-sort '(9 5 3 7 6 0 1 2 8 4 9))
#?=(selection-sort '(9 5 3 7 6 0 1 2 8 4 9) >)
