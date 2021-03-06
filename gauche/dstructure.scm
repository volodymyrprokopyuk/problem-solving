(define-module dstructure)

(select-module dstructure)

;; <avector> array vector

(define-class <avector> ()
  ([capacity :init-keyword :capacity :init-value 10 :getter capacity]
   [array :accessor array]
   [length :init-value 0 :accessor length]
   [position :init-value 0 :accessor position]))

(define-method initialize ([v <avector>] _)
  "Initializes the vector v with the storage capacity"
  (next-method)
  (set! (array v) (make-vector (capacity v))))

(define-method write-object ([v <avector>] p)
  "Writes the representation of the vector v to the porte p"
  (format p "#<~a ~a of ~a at ~a as ~a>"
          (class-name (current-class-of v))
          (length v) (capacity v) (position v)
          (vector-copy (array v) 0 (length v))))

(define-method empty? ([v <avector>])
  "Returns #t if the vector v is empty, otherwise #f. O(1)"
  [zero? (length v)])

(define-method next! ([v <avector>])
  "Moves the current position to the next element. O(1)"
  (when [< (position v) (length v)] (inc! (position v))))

(define-method previous! ([v <avector>])
  "Moves the current position to the previous element. O(1)"
  (when [> (position v) 0] (dec! (position v))))

(define-method start! ([v <avector>])
  "Moves the current position to the start of the vector v. O(1)"
  (set! (position v) 0))

(define-method end! ([v <avector>])
  "Moves the current position to the end of the vector v. O(1)"
  (set! (position v) (- (length v) 1)))

(define-method position! ([v <avector>] p)
  "Moves the current postion of the vector v to the p. O(1)"
  (when (or [< p 0] [>= p (length v)])
    (error "<avector> position!: position out of range"))
  (set! (position v) p))

(define-method value ([v <avector>])
  "Returns the element of the vector v at the current position. O(1)"
  (when [empty? v] (error "<avector> value: empty vector"))
  (~ (array v) (position v)))

(define-method insert! ([v <avector>] e)
  "Inserts the element e into the vector v at the current position. O(n)"
  (when [= (length v) (capacity v)] (error "<avector> insert!: exceeded capacity"))
  (let ([l (length v)] [p (position v)] [a (array v)])
    (do ([i (- l 1) (- i 1)]) ([< i p]) (set! (~ a (+ i 1)) (~ a i)))
    (set! (~ a p) e) (inc! (length v))))

(define-method update! ([v <avector>] e)
  "Updates the element at the current position of the vector v with e. O(1)"
  (when [empty? v] (error "<avector> update!: empty vector"))
  (let ([a (array v)] [p (position v)])
    (set! (~ a p) e)))

(define-method remove! ([v <avector>])
  "Removes the element from the vector v at the current position. O(n)"
  (when [empty? v] (error "<avector> remove!: empty vector"))
  (let ([l (length v)] [p (position v)] [a (array v)] [w (value v)])
    (do ([i (+ p 1) (+ i 1)]) ([= i l]) (set! (~ a (- i 1)) (~ a i)))
    (dec! (length v)) w))

(define-method clear! ([v <avector>])
  "Clears the vector v. O(1)"
  (set! (position v) 0) (set! (length v) 0))

;; <node>

(define-class <node> ()
  ([value :init-keyword :value :accessor value]
   [next :init-keyword :next :accessor next]
   [previous :init-keyword :previous :accessor previous]))

(define (null-node)
  "Returns the null node"
  (make <node> :value #f :next #f :previous #f))

(define-method fold-next (f s [n <node>])
  "Left folds next the function f over the nodes n starting with the seed s"
  (let fold* ([n n] [r s])
    (cond
      [(not (next n)) (reverse (f n r))]
      [else (fold* (next n) (f n r))])))

;; <llist> linked list

(define-class <llist> ()
  ([head :accessor head]
   [current :accessor current]
   [tail :accessor tail]
   [length :init-value 0 :accessor length]
   [position :init-value 0 :accessor position]))

(define-method initialize ([l <llist>] _)
  "Initializes the list l with the null node"
  (next-method)
  (let ([n (null-node)])
    (set! (head l) n) (set! (current l) n) (set! (tail l) n)))

(define-method write-object ([l <llist>] p)
  (define (kons n s)
    (let ([v (value n)]
          [head? (eq? n (head l))]
          [current? (eq? n (current l))]
          [tail? (eq? n (tail l))])
      (cond
        [(and head? current? tail?) (cons (cons v 'hct) s)]
        [(and head? current?) (cons (cons v 'hc) s)]
        [(and current? tail?) (cons (cons v 'ct) s)]
        [head? (cons (cons v 'h) s)]
        [current? (cons (cons v 'c) s)]
        [tail? (cons (cons v 't) s)]
        [else (cons v s)])))
  "Writes the representaion of the list l to the port p"
  (format p "#<~a ~a at ~a as ~a>"
          (class-name (current-class-of l))
          (length l) (position l) (fold-next kons '() (head l))))

(define-method empty? ([l <llist>])
  "Returns #t if the list is empty, otherwise #f. O(1)"
  [zero? (length l)])

(define-method next! ([l <llist>])
  "Moves the current position to the next element. O(1)"
  (let ([c (current l)])
    (unless [eq? (next c) (tail l)]
      (set! (current l) (next c)) (inc! (position l)))))

(define-method previous! ([l <llist>])
  "Moves the current posiiton to the previous element. O(n)"
  (let ([c (current l)])
    (unless [eq? c (head l)]
      (let forward* ([n (head l)] [i 0])
        (cond
          [(eq? (next n) c) (set! (current l) n) (set! (position l) i)]
          [else (forward* (next n) (+ i 1))])))))

(define-method start! ([l <llist>])
  "Moves the current position to the start of the list l. O(1)"
  (set! (current l) (head l)) (set! (position l) 0))

(define-method end! ([l <llist>])
  "Moves the current position to the end of the list l. O(n)"
  (let ([c (current l)] [t (tail l)])
    (unless [eq? (next c) t]
      (let forward* ([n (head l)] [i 0])
        (cond
          [(eq? (next n) t) (set! (current l) n) (set! (position l) i)]
          [else (forward* (next n) (+ i 1))])))))

(define-method position! ([l <llist>] p)
  "Moves the current position of the list l to the p. O(n)"
  (when (or [< p 0] [>= p (length l)])
    (error "<llist> position!: position out of range"))
  (unless [= p (position l)]
    (let forward* ([n (head l)] [i -1])
      (cond
        [(= (+ i 1) p) (set! (current l) n) (set! (position l) (+ i 1))]
        [else (forward* (next n) (+ i 1))]))))

(define-method value ([l <llist>])
  "Returns the velue of the list l at the current position. O(1)"
  (when [empty? l] (error "<llsit> value: empty list"))
  (value (next (current l))))

(define-method insert! ([l <llist>] v)
  "Inserts the value v into the list l at the current position. O(1)"
  (let* ([c (current l)] [n (make <node> :value v :next (next c))])
    (set! (next c) n)
    (when [eq? c (tail l)] (set! (tail l) (next c)))
    (inc! (length l))))

(define-method update! ([l <llist>] v)
  "Updates the element at the cuturrent position of the list l with v. O(1)"
  (when [empty? l] (error "<llist> update!: empty list"))
  (let ([c (current l)])
    (set! (value (next c)) v)))

(define-method remove! ([l <llist>])
  "Removes the element from the list l at the current position. O(1)"
  (when [empty? l] (error "<llist> remove!: empty list"))
  (let ([c (current l)] [v (value l)])
    (when [eq? (next c) (tail l)] (set! (tail l) c))
    (set! (next c) (next (next c)))
    (dec! (length l)) v))

(define-method clear! ([l <llist>])
  "Clears the list l. O(1)"
  (let ([n (null-node)])
    (set! (head l) n) (set! (current l) n) (set! (tail l) n)
    (set! (position l) 0) (set! (length l) 0)))

;; (let ([x (make <avector> :capacity 5)])
(let ([x (make <llist>)])
  (print x)
  #?=(empty? x)
  (insert! x 1)
  (insert! x 2)
  (insert! x 3)
  (insert! x 4)
  (insert! x 5)
  (print x)
  #?=(empty? x)
  (position! x 2)
  (print x)
  #?=(value x)
  (next! x)
  (print x)
  #?=(value x)
  (previous! x)
  (print x)
  #?=(value x)
  (start! x)
  (print x)
  #?=(value x)
  (end! x)
  (print x)
  #?=(value x)
  (update! x 10)
  (print x)
  #?=(remove! x)
  (end! x)
  (print x)
  #?=(remove! x)
  (start! x)
  (print x)
  #?=(remove! x)
  (print x)
  (clear! x)
  (print x))
