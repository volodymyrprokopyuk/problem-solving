(define (make-node d c)
  "Returns a node (tree) with the datum d and children c"
  (list d c))

(define (make-leaf d)
  "Returns a node with the datum d and no children"
  (list d '()))

;; #?=(make-node 'a (map make-leaf '(b c)))

(define (datum n)
  "Returns datum of the node n"
  (car n))

;; #?=(datum (make-node 'a (map make-leaf '(b c))))

(define (children n)
  "Returns children of the node n"
  (cadr n))

;; #?=(children (make-node 'a (map make-leaf '(b c))))

(define (leaf? n)
  "Returns #t if the node n is a leaf (does not have children)"
  (null? (children n)))

(define (count-leaves n)
  "Returns the numbe of leaves in the node n"
  (cond
    [(leaf? n) 1]
    [else (apply + (map count-leaves (children n)))]))

(let ([n (make-node 'a
                    (list
                     (make-node 'b
                                (list
                                 (make-node 'd (map make-leaf '(g h)))
                                 (make-node 'e (map make-leaf '(i j)))))
                     (make-node 'c
                                (list
                                 (make-node 'f (map make-leaf '(k l m)))))))])
  #?=(count-leaves n))
