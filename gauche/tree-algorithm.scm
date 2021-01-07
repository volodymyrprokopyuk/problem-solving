(define (make-node d c)
  "Returns a node (tree) with the datum d and children c"
  (cons d c))

(define (make-leaf d)
  "Returns a node with the datum d and no children"
  (list d))

;; #?=(make-node 'a (map make-leaf '(b c)))

(define (datum n)
  "Returns datum of the node n"
  (car n))

;; #?=(datum (make-node 'a (map make-leaf '(b c))))

(define (children n)
  "Returns children of the node n"
  (cdr n))

;; #?=(children (make-node 'a (map make-leaf '(b c))))

(define (leaf? n)
  "Returns #t if the node n is a leaf (does not have children)"
  (null? (children n)))

(define (count-leaves n)
  "Returns the number of leaves in the node n (higher-order function map)"
  (cond
    [(leaf? n) 1]
    [else (apply + (map count-leaves (children n)))]))

(define (count-leaves2 n)
  "Returns the number of leaves in the node n (mutual recursion)"
  (cond
    [(leaf? n) 1]
    [else
     (let count* ([c (children n)])
       (cond
         [(null? c) 0]
         [else (+ (count-leaves2 (car c)) (count* (cdr c)))]))]))

(define (in-tree? d n :optional (e eq?))
  "Returns the first node of the tree n whose datum is equal to the datum d"
  (cond
    [(e (datum n) d) n]
    [else (any (cut in-tree? d <> e) (children n))]))

(define (in-tree2? d n :optional (e eq?))
  "Returns the first node of the tree n whose datum is equal to the datum d"
  (cond
    [(e (datum n) d) n]
    [else
     (let in-tree* ([c (children n)])
       (cond
         [(null? c) #f]
         [else (or (in-tree2? d (car c) e) (in-tree* (cdr c)))]))]))

(define (locate d n :optional (e eq?))
  "Returns a path to the node whose datum is equal to the datum d, otherwise '()"
  (reverse
   (let locate* ([n n] [r '()])
     (cond
       [(e (datum n) d) (cons (datum n) r)]
       [else (any (cut locate* <> (cons (datum n) r)) (children n))]))))

#;(let ([n (make-node 'a
                    (list
                     (make-node 'b
                                (list
                                 (make-node 'd (map make-leaf '(g h)))
                                 (make-node 'e (map make-leaf '(i j)))))
                     (make-node 'c
                                (list
                                 (make-node 'f (map make-leaf '(k l m)))))))])
  #?=(count-leaves n)
  #?=(count-leaves2 n)
  #?=(in-tree? 'e n)
  #?=(in-tree2? 'e n)
  #?=(locate 'l n))

(define (parse-arith e :optional (p '((+ . 1) (- . 1) (* . 2) (/ . 2))))
  "Parses arithmetic expression as per the presendece p and returns a parse tree"
  (let parse* ([e e] [o '()] [n '()])
    (cond
      [(null? e)
       (let operate* ([o o] [n n])
         (cond
           [(null? o) (if [null? n] n (car n))]
           [else (operate* (cdr o)
                           (cons (make-node (car o) (list (cadr n) (car n)))
                                 (cddr n)))]))]
      [(number? (car e)) (parse* (cdr e) o (cons (make-leaf (car e)) n))]
      [(memq (car e) '(+ - * /))
       (let operate* ([o o] [n n])
         (cond
           [(and (not (null? o)) (<= (assq-ref p (car e)) (assq-ref p (car o))))
            (operate* (cdr o)
                      (cons (make-node (car o) (list (cadr n) (car n)))
                            (cddr n)))]
           [else (parse* (cdr e) (cons (car e) o) n)]))]
      [(pair? (car e)) (parse* (cdr e) o (cons (parse-arith (car e) p) n))]
      [else (error #"invalid term ~(car e)")])))

;; #?=(parse-arith '())
;; #?=(parse-arith '(1))
;; #?=(parse-arith '(1 + 2))
;; #?=(parse-arith '(1 + 2 * 3))
;; #?=(parse-arith '(1 * 2 + 3))
;; #?=(parse-arith '(1 + 2 * (3 - 4 / 5)))
;; #?=(parse-arith '(4 + 3 * 7 - 5 / (3 + 4) + 6))

(define (evaluate-arith n)
  "Evalues the parse tree n of the arithmetic expression"
  (cond
    [(number? (datum n)) (datum n)]
    [else
     (case (datum n)
       [(+) (+ (evaluate-arith (car (children n)))
               (evaluate-arith (cadr (children n))))]
       [(-) (- (evaluate-arith (car (children n)))
               (evaluate-arith (cadr (children n))))]
       [(*) (* (evaluate-arith (car (children n)))
               (evaluate-arith (cadr (children n))))]
       [(/) (/ (evaluate-arith (car (children n)))
               (evaluate-arith (cadr (children n))))])]))

;; #?=($ evaluate-arith $ parse-arith '(1))
;; #?=($ evaluate-arith $ parse-arith '(1 + 2))
;; #?=($ evaluate-arith $ parse-arith '(1 + 2 * 3))
;; #?=($ evaluate-arith $ parse-arith '(1 * 2 + 3))
;; #?=($ evaluate-arith $ parse-arith '(1 + 2 * (3 - 4 / 5)))
;; #?=($ evaluate-arith $ parse-arith '(4 + 3 * 7 - 5 / (3 + 4) + 6))
