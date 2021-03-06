(use gauche.unicode) ;; string-downcase

(define (repeat f n)
  "Composes the function f n times"
  (cond
    [(zero? n) identity]
    [(= n 1) f]
    ;; [else (lambda (x) (f ((repeat f (- n 1)) x)))]))
    [else (lambda (x) ((repeat f (- n 1)) (f x)))]))

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
  "Returns a sorted copy of the list l using selection sort"
  (let sort* ([l l] [r '()])
    (cond
      [(null? l) (reverse r)]
      [else
       (let* ([m (reduce (lambda (e s) (if [c e s] e s)) #f l)]
              [k (remove1 m l eqv?)])
         (sort* k (cons m r)))])))

;; #?=(selection-sort '())
;; #?=(selection-sort '(9 5 3 7 6 0 1 2 8 4 9))
;; #?=(selection-sort '(9 5 3 7 6 0 1 2 8 4 9) >)

(define (=merge x y :optional (c <))
  "Merges the lists x and y as per the comparator c"
  (let merge* ([x x] [y y] [r '()])
    (cond
      [(null? x) (append (reverse r) y)]
      [(null? y) (append (reverse r) x)]
      [(c (car x) (car y)) (merge* (cdr x) y (cons (car x) r))]
      [else (merge* x (cdr y) (cons (car y) r))])))

;; #?=(=merge '(1 3 5 6 7 8) '(0 2 4 6 7 8 9))

(define (merge-sort l :optional (c <))
  "Returns a sorted copy of the list l using merge sort"
  (cond
    [(< (length l) 2) l]
    [else
     (let ([m (floor (/ (length l) 2))])
       (=merge (merge-sort (take l m) c) (merge-sort (drop l m) c) c))]))

;; #?=(merge-sort '(9 5 3 7 6 0 1 2 8 4 9))
;; #?=(merge-sort '(9 5 3 7 6 0 1 2 8 4 9) >)

(define (subsets l)
  "Returns all subsets of the list l"
  (cond
    [(null? l) '(())]
    [else
     (let ([s (subsets (cdr l))])
       (append (map (lambda (e) (cons (car l) e)) s) s))]))

;; #?=(subsets '())
;; #?=(subsets '(c))
;; #?=(subsets '(b c))
;; #?=(subsets '(a b c))

(define (palindrome? s)
  "Returns #t if the string s is a palindrome"
  (let pal* ([l (remove #[ .,?!] (string->list (string-downcase s)))])
    (cond
      [(< (length l) 2) #t]
      [(char=? (car l) (last l)) (pal* (cdr (drop-right l 1)))]
      [else #f])))

;; #?=(palindrome? "Flee to me remote Elf")
;; #?=(palindrome? "Mr. Owl ate my metal worm!")
;; #?=(palindrome? "Was it a car or a cat I saw?")

(define (deep-fold s f l)
  "Left folds deeplby the function f over the list l staring with the seed s"
  (let fold* ([s s] [l l])
    (cond
      [(null? l) s]
      [(pair? (car l)) (fold* (fold* s (car l)) (cdr l))]
      [else (fold* (f s (car l)) (cdr l))])))

;; #?=(deep-fold 0 (lambda (s _) (+ s 1)) '(1 2 (3 (4 5) 6) 7 8))
;; #?=(deep-fold 0 + '(1 2 (3 (4 5) 6) 7 8))
;; #?=(deep-fold 0 - '(1 2 (3 (4 5) 6) 7 8))

(define (=max . l)
  "Returns the maximum of the arguments l"
  (when [null? l] (error "=max: empty list"))
  (let max* ([l (cdr l)] [r (car l)])
    (cond
      [(null? l) r]
      [else (max* (cdr l) (if [> (car l) r] (car l) r))])))

;; #?=(=max 1)
;; #?=(=max 1 2)
;; #?=(=max 1 2 5 3 4)

(define (=append . l)
  "Appends the lists l into a single list"
  (let append* ([l (reverse l)] [r '()])
    (cond
      [(null? l) r]
      [else
       (let append1* ([m (reverse (car l))] [r r])
         (cond
           [(null? m) (append* (cdr l) r)]
           [else (append1* (cdr m) (cons (car m) r))]))])))

;; #?=(=append)
;; #?=(=append '(a b))
;; #?=(=append '(a b) '(c))
;; #?=(=append '(a b) '(c d e) '() '(f (g h)))

(define =append2
  (case-lambda
    [() '()]
    [(a) a]
    [(a b)
     (let append* ([a (reverse a)] [b b])
       (cond
         [(null? a) b]
         [else (append* (cdr a) (cons (car a) b))]))]
    [(a b . c) (apply =append2 (=append2 a b) c)]))

;; #?=(=append2)
;; #?=(=append2 '(a b))
;; #?=(=append2 '(a b) '(c))
;; #?=(=append2 '(a b) '(c d e) '() '(f (g h)))

(define (flatten l)
  "Flattens the list l"
  (let flatten* ([l l] [r '()])
    (cond
      [(null? l) (reverse r)]
      [(pair? (car l)) (flatten* (cdr l) (reverse (flatten* (car l) r)))]
      [else (flatten* (cdr l) (cons (car l) r))])))

;; #?=(flatten '(a b (c (d e (f g) h i) j k) l m))

(define (flatten2 l)
  "Flattens the list l"
  (let flatten* ([l l] [r '()])
    (cond
      [(null? l) r]
      [(pair? (car l)) (flatten* (car l) (flatten* (cdr l) r))]
      [else (cons (car l) (flatten* (cdr l) r))])))

;; #?=(flatten2 '(a b (c (d e (f g) h i) j k) l m))

(define (flatten3 l)
  (cond
    [(null? l) '()]
    [(pair? (car l)) (append (flatten3 (car l)) (flatten3 (cdr l)))]
    [else (cons (car l) (flatten3 (cdr l)))]))

;; #?=(flatten3 '(a b (c (d e (f g) h i) j k) l m))

(define (deep-map f l)
  "Maps the funciton f over the nested list l"
  (cond
    [(null? l) '()]
    [(pair? (car l)) (cons (deep-map f (car l)) (deep-map f (cdr l)))]
    [else (cons (f (car l)) (deep-map f (cdr l)))]))

;; #?=(deep-map (cut * <> 10) '(1 2 (3 (4 5) 6 7) 8 9))

(define (=fold f s l)
  "Folds left the function f starting with the seed s over the list l"
  ;; (print s " " l)
  (cond
    [(null? l) s]
    [else (=fold f (f (car l) s) (cdr l))]))

;; #?=(fold cons '() '(a b c d e))
;; #?=(=fold cons '() '(a b c d e))
;; #?=(fold - 0 '(1 2 3 4 5))
;; #?=(=fold - 0 '(1 2 3 4 5))

(define (=fold-right f s l)
  "Folds right the function f starting with the seed s over the list l"
  (cond
    [(null? l) s]
    [else (f (car l) (=fold-right f s (cdr l)))]))

;; #?=(fold-right cons '() '(a b c d e))
;; #?=(=fold-right cons '() '(a b c d e))
;; #?=(fold-right - 0 '(1 2 3 4 5))
;; #?=(=fold-right - 0 '(1 2 3 4 5))

(define (deep-fold f s l)
  "Folds left the function f over the nexted list"
  (cond
    [(null? l) s]
    [(pair? (car l)) (deep-fold f (deep-fold f s (car l)) (cdr l))]
    [else (deep-fold f (f (car l) s) (cdr l))]))

;; #?=(deep-fold cons '() '(a b (c d (e (f g) h i) j k) l))
;; #?=(deep-fold + 0 '(1 2 (3 4 (5) 6) 7))

(define (deep-fold-right f s l)
  "Folds right the function f over the nexted list"
  (cond
    [(null? l) s]
    [(pair? (car l)) (f (deep-fold-right f s (car l)) (deep-fold-right f s (cdr l)))]
    [else (f (car l) (deep-fold-right f s (cdr l)))]))

;; #?=(deep-fold-right cons '() '(a b (c d (e (f g) h i) j k) l))
;; #?=(deep-fold-right + 0 '(1 2 (3 4 (5) 6) 7))

(define (=compose . l)
  "Composes the list l of functions"
  (cond
    [(null? l) identity]
    [(= (length l) 1) (car l)]
    [else (lambda x ((car l) (apply (apply =compose (cdr l)) x)))]))

;; #?=((=compose) '(a b c))
;; #?=((=compose car) '(a b c))
;; #?=((=compose car cdr) '(a b c))
;; #?=((=compose car cdr cdr) '(a b c))
;; #?=((=compose car cdr cdr cons) 'a '(b c))

(define (depth l)
  "Returns the depth of the nested list l"
  (cond
    [(null? l) 0]
    [(pair? (car l)) (max (+ (depth (car l)) 1) (depth (cdr l)))]
    [else (max (depth (cdr l)) 1)]))

;; #?=(depth '())
;; #?=(depth '(a))
;; #?=(depth '(a b))
;; #?=(depth '(a (b) (c (d))))
;; #?=(depth '(a (b) (c (d (e)))))

(define (depth2 a)
  "Returns the depth of the nested list l"
  (cond
    [(pair? a) (+ (apply max (map depth2 a)) 1)]
    [else 0]))

;; #?=(depth2 '())
;; #?=(depth2 '(a))
;; #?=(depth2 '(a b))
;; #?=(depth2 '(a (b) (c (d))))
;; #?=(depth2 '(a (b) (c (d (e)))))

(define (=map f l . m)
  "Maps the function f over the variadic arguments l"
  (let map* ([a (cons l m)] [r '()])
    (cond
      [(any null? a) (reverse r)]
      [else (map* (map cdr a) (cons (apply f (map car a)) r))])))

;; #?=(=map (cut + <> 1) '())
;; #?=(=map (cut + <> 1) '(1 2 3 4))
;; #?=(=map + '(1 2 3 4) '(10 20 30 40))
;; #?=(=map - '(1 2 3 4) '(10 20 30 40))
;; #?=(=map cons '(a b c d) '(A B C D))

(define (=last l)
  "Returns the last element from the list l"
  (cond
    [(null? (cdr l)) (car l)]
    [else (=last (cdr l))]))

;; #?=(=last '())
;; #?=(=last '(a))
;; #?=(=last '(a b))

(define (triangle-number n)
  "Returns the number of squares needed to construct a stairs of high n"
  (cond
    [(zero? n) 0]
    [else (+ n (triangle-number (- n 1)))]))

;; #?=(triangle-number 0)
;; #?=(triangle-number 1)
;; #?=(triangle-number 2)
;; #?=(triangle-number 3)
;; #?=(triangle-number 4)
;; #?=(triangle-number 5)

(define (triangle-number2 n)
  "Returns the number of squares needed to construct a stairs of high n"
  (let tnum* ([i 0] [r 0])
    (cond
      [(> i n) r]
      [else (tnum* (+ i 1) (+ i r))])))

;; #?=(triangle-number2 0)
;; #?=(triangle-number2 1)
;; #?=(triangle-number2 2)
;; #?=(triangle-number2 3)
;; #?=(triangle-number2 4)
;; #?=(triangle-number2 5)

(define (alist-remove k al :optional (c eq?))
  "Removes all occurences of the key k from the alist al"
  (let rem* ([al (reverse al)] [r '()])
    (cond
      [(null? al) r]
      [(c k (caar al)) (rem* (cdr al) r)]
      [else (rem* (cdr al) (cons (car al) r))])))

;; #?=(alist-remove 'b '((a . 1) (b . 2) (c . 3) (b . 4) (d . 5)))

(define (alist-remove2 k al :optional (c eq?))
  "Removes all occurences of the key k from the alist al"
  (do ([al (reverse al) (cdr al)]
       [r '() (if [c k (caar al)] r (cons (car al) r))])
      ([null? al] r)))

;; #?=(alist-remove2 'b '((a . 1) (b . 2) (c . 3) (b . 4) (d . 5)))

(define (tree-sub a b l :optional (c eq?))
  "Substitutes the element a for the element b in the tree l"
  (cond
    [(null? l) '()]
    [(pair? (car l)) (cons (tree-sub a b (car l)) (tree-sub a b (cdr l)))]
    [(c (car l) a) (cons b (tree-sub a b (cdr l)))]
    [else (cons (car l) (tree-sub a b (cdr l)))]))

;; #?=(tree-sub 'a 'A '(a b (c a (d a a (a) e) f a) a h))

(define (tree-remove a l :optional (c eq?))
  "Removes the element a from the tree l"
  (cond
    [(null? l) '()]
    [(pair? (car l)) (cons (tree-remove a (car l)) (tree-remove a (cdr l)))]
    [(c (car l) a) (tree-remove a (cdr l))]
    [else (cons (car l) (tree-remove a (cdr l)))]))

;; #?=(tree-remove 'a '(a b (c a (d a a (a) e) f a) a h))
