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
  (let ([w (vector-copy (array v) 0 (length v))] [i (position v)])
    (unless [empty? v] (set! (~ w i) (cons (~ w i) 'c)))
    (format p "#<~a ~a of ~a at ~a as ~a>"
            (class-name (current-class-of v))
            (length v) (capacity v) (position v) w)))

(define-method empty? ([v <avector>])
  "Returns #t if the vector v is empty, otherwise #f. O(1)"
  [zero? (length v)])

(define-method next! ([v <avector>])
  "Moves the current position to the next element. O(1)"
  (when [< (position v) (- (length v) 1)] (inc! (position v))))

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
  (when [>= (length v) (capacity v)] (error "<avector> insert!: exceeded capacity"))
  (let ([l (length v)] [p (position v)] [a (array v)])
    (do ([i (- l 1) (- i 1)]) ([< i p]) (set! (~ a (+ i 1)) (~ a i)))
    (set! (~ a p) e)
    (inc! (length v))))

(define-method append! ([v <avector>] e)
  "Appends the element e at the end of the vector v. O(1)"
  (when [>= (length v) (capacity v)] (error "<avector> insert!: exceeded capacity"))
  (let ([l (length v)] [a (array v)])
    (set! (~ a l) e)
    (inc! (length v))))

(define-method update! ([v <avector>] e)
  "Updates the element at the current position of the vector v with e. O(1)"
  (when [empty? v] (error "<avector> update!: empty vector"))
  (set! (~ (array v) (position v)) e))

(define-method remove! ([v <avector>])
  "Removes the element from the vector v at the current position. O(n)"
  (when [empty? v] (error "<avector> remove!: empty vector"))
  (let ([l (length v)] [p (position v)] [a (array v)] [w (value v)])
    (do ([i (+ p 1) (+ i 1)]) ([= i l]) (set! (~ a (- i 1)) (~ a i)))
    (unless [zero? (position v)] (dec! (position v)))
    (dec! (length v))
    w))

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

(define-method node-fold-next (f s [n <node>])
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
  "Initializes the head, current and tail nodes"
  (next-method)
  (let ([h (null-node)] [t (null-node)])
    (set! (head l) h) (set! (current l) h) (set! (tail l) t) (set! (next h) t)))

(define ((node-mark-hct l) n s)
  "Marks the head, the current and the tail nodes in the list of nodes l"
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

(define-method write-object ([l <llist>] p)
  "Writes the representaion of the list l to the port p"
  (format p "#<~a ~a at ~a as ~a>"
          (class-name (current-class-of l))
          (length l) (position l)
          (node-fold-next (node-mark-hct l) '() (head l))))

(define-method empty? ([l <llist>])
  "Returns #t if the list is empty, otherwise #f. O(1)"
  [zero? (length l)])

(define-method next! ([l <llist>])
  "Moves the current position to the next element. O(1)"
  (let ([c (current l)])
    (unless [eq? (next (next c)) (tail l)]
      (set! (current l) (next c))
      (inc! (position l)))))

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
  (set! (current l) (head l))
  (set! (position l) 0))

(define-method end! ([l <llist>])
  "Moves the current position to the end of the list l. O(n)"
  (let ([c (current l)] [t (tail l)])
    (unless [eq? (next (next c)) t]
      (let forward* ([n c] [i (position l)])
        (cond
          [(eq? (next (next n)) t) (set! (current l) n) (set! (position l) i)]
          [else (forward* (next n) (+ i 1))])))))

(define-method position! ([l <llist>] p)
  "Moves the current position of the list l to the p. O(n)"
  (when (or [< p 0] [>= p (length l)])
    (error "<llist> position!: position out of range"))
  (let ([q (position l)])
    (unless [= p q]
      (let forward* ([n (if [< p q] (head l) (current l))] [i (if [< p q] 0 q)])
        (cond
          [(= i p) (set! (current l) n) (set! (position l) i)]
          [else (forward* (next n) (+ i 1))])))))

(define-method value ([l <llist>])
  "Returns the value of the list l at the current position. O(1)"
  (when [empty? l] (error "<llsit> value: empty list"))
  (value (next (current l))))

(define-method insert! ([l <llist>] v)
  "Inserts the value v into the list l at the current position. O(1)"
  (let* ([c (current l)] [n (make <node> :value v :next (next c))])
    (set! (next c) n)
    (inc! (length l))))

(define-method append! ([l <llist>] v)
  "Appends the value v at the end of the list l. O(n)"
  (cond
    [(empty? l) (insert! l v)]
    [else
     (end! l)
     (let ([c (current l)] [n (make <node> :value v :next (tail l))])
       (set! (next (next c)) n)
       (inc! (length l)))]))

(define-method update! ([l <llist>] v)
  "Updates the element at the cuturrent position of the list l with v. O(1)"
  (when [empty? l] (error "<llist> update!: empty list"))
  (set! (value (next (current l))) v))

(define-method remove! ([l <llist>])
  "Removes the element at the current position from the list l. O(1)"
  (when [empty? l] (error "<llist> remove!: empty list"))
  (let ([c (current l)] [v (value l)])
    (set! (next c) (next (next c)))
    (dec! (length l))
    v))

(define-method clear! ([l <llist>])
  "Clears the list l. O(1)"
  (let ([h (null-node)] [t (null-node)])
    (set! (head l) h) (set! (current l) h) (set! (tail l) t) (set! (next h) t)
    (set! (position l) 0) (set! (length l) 0)))

;; <dlist> doubly linked list

(define-class <dlist> ()
  ([head :accessor head]
   [current :accessor current]
   [tail :accessor tail]
   [length :init-value 0 :accessor length]
   [position :init-value 0 :accessor position]))

(define-method initialize ([l <dlist>] _)
  "Initializes the head, current and tail nodes"
  (next-method)
  (let ([h (null-node)] [t (null-node)])
    (set! (head l) h) (set! (current l) h) (set! (tail l) t)
    (set! (next h) t) (set! (previous t) h)))

(define-method write-object ([l <dlist>] p)
  "Writes the representaion of the list l to the port p"
  (format p "#<~a> ~a at ~a as ~a"
          (class-name (current-class-of l))
          (length l) (position l)
          (node-fold-next (node-mark-hct l) '() (head l))))

(define-method empty? ([l <dlist>])
  "Returns #t if the list is empty, otherwise #f. O(1)"
  [zero? (length l)])

(define-method next! ([l <dlist>])
  "Moves the current position to the next element. O(1)"
  (let ([c (current l)])
    (unless [eq? (next c) (tail l)]
      (set! (current l) (next c))
      (inc! (position l)))))

(define-method previous! ([l <dlist>])
  "Moves the current posiiton to the previous element. O(1)"
  (let ([c (current l)])
    (unless [eq? c (head l)]
      (set! (current l) (previous c))
      (dec! (position l)))))

(define-method start! ([l <dlist>])
  "Moves the current position to the start of the list l. O(1)"
  (set! (current l) (if [empty? l] (head l) (next (head l))))
  (set! (position l) 0))

(define-method end! ([l <dlist>])
  "Moves the current position to the end of the list l. O(1)"
  (set! (current l) (previous (tail l)))
  (set! (position l) (- (length l) 1)))

(define-method position! ([l <dlist>] p)
  "Moves the current position of the list l to the p. O(n)"
  (when (or [< p 0] [>= p (length l)])
    (error "<dlist> position!: position out of range"))
  (cond
    [(> p (position l))
     (let forward* ([n (current l)] [i (position l)])
       (cond
         [(= i p) (set! (current l) n) (set! (position l) i)]
         [else (forward* (next n) (+ i 1))]))]
    [(< p (position l))
     (let backward* ([n (current l)] [i (position l)])
       (cond
         [(= i p) (set! (current l) n) (set! (position l) i)]
         [else (backward* (previous n) (- i 1))]))]))

(define-method value ([l <dlist>])
  "Returns the velue of the list l at the current position. O(1)"
  (when [empty? l] (error "<dlist> value: empty list"))
  (value (current l)))

(define-method insert! ([l <dlist>] v)
  "Inserts the value v into the list l at the current position. O(1)"
  (cond
    [(empty? l) (append! l v) (set! (current l) (next (head l)))]
    [else
     (let* ([c (current l)] [n (make <node> :value v :previous (previous c) :next c)])
       (set! (next (previous c)) n)
       (set! (previous c) n)
       (set! (current l) n)
       (inc! (length l)))]))

(define-method append! ([l <dlist>] v)
  "Appends the value v at the end of the list l. O(1)"
  (let* ([t (tail l)] [e (previous t)] [n (make <node> :value v :previous e :next t)])
    (set! (next e) n)
    (set! (previous t) n)
    (inc! (length l))))

(define-method update! ([l <dlist>] v)
  "Updates the element at the cuturrent position of the list l with v. O(1)"
  (when [empty? l] (error "<dlist> update!: empty list"))
  (set! (value (current l)) v))

(define-method remove! ([l <dlist>])
  "Removes the element at the current position from the list l. O(1)"
  (when [empty? l] (error "<dlist> remove!: empty list"))
  (let ([c (current l)] [v (value l)])
    (set! (current l) (previous c))
    (unless [zero? (position l)] (dec! (position l)))
    (set! (next (current l)) (next c))
    (set! (previous (next c)) (current l))
    (dec! (length l))
    (when (and (not [empty? l]) (eq? (current l) (head l)))
      (set! (current l) (next (head l))))
    v))

(define-method clear! ([l <dlist>])
  "Clears the list l. O(1)"
  (let ([h (null-node)] [t (null-node)])
    (set! (head l) h) (set! (current l) h) (set! (tail l) t)
    (set! (next h) t) (set! (previous t) h)
    (set! (position l) 0) (set! (length l) 0)))

;; <astack> array stack

(define-class <astack> ()
  ([capacity :init-keyword :capacity :init-value 10 :getter capacity]
   [array :accessor array]
   [length :init-value 0 :accessor length]))

(define-method initialize ([s <astack>] _)
  "Initializes the stack s capacity"
  (next-method)
  (set! (array s) (make-vector (capacity s))))

(define-method write-object ([s <astack>] p)
  "Writes the representation of the stack s to the port p"
  (format p "#<~a ~a as ~a>"
          (class-name (current-class-of s))
          (length s) (vector-copy (array s) 0 (length s))))

(define-method empty? ([s <astack>])
  "Returns #t if the stack s is empty, otherwise #f. O(1)"
  [zero? (length s)])

(define-method push! ([s <astack>] e)
  "Pushes the element e on top of the stack s. O(1)"
  (when [>= (length s) (capacity s)] (error "<astack> push!: exceeded capacity"))
  (set! (~ (array s) (length s)) e)
  (inc! (length s)))

(define-method peek ([s <astack>])
  "Returns the top element of the stack s. O(1)"
  (when [empty? s] (error "<astack> peek: empty stack"))
  (~ (array s) (- (length s) 1)))

(define-method pop! ([s <astack>])
  "Removes the top element from the stack s. O(1)"
  (when [empty? s] (error "<astack> peek: empty stack"))
  (dec! (length s))
  (~ (array s) (length s)))

(define-method clear! ([s <astack>])
  "Clears the stack s. O(1)"
  (set! (length s) 0))

;; <lstack> linked list stack

(define-class <lstack> ()
  ([top :init-form (make <node> :value #f :next #f) :accessor top]
   [length :init-value 0 :accessor length]))

(define-method write-object ([s <lstack>] p)
  "Writes the representation of the stack s to the port p"
  (format p "#<~a ~a as ~a>"
          (class-name (current-class-of s))
          (length s) (node-fold-next (lambda (n r) (cons (value n) r)) '() (top s))))

(define-method empty? ([s <lstack>])
  "Returns #t if the stack s is empty, otherwise #f. O(1)"
  [zero? (length s)])

(define-method push! ([s <lstack>] v)
  "Pushes the value v on top of the stack s. O(1)"
  (let ([n (make <node> :value v :next (top s))])
    (set! (top s) n)
    (inc! (length s))))

(define-method peek ([s <lstack>])
  "Returns the top element of the stack s. O(1)"
  (when [empty? s] (error "<lstack> peek: empty stack"))
  (value (top s)))

(define-method pop! ([s <lstack>])
  "Removes the top element from the stack s. O(1)"
  (when [empty? s] (error "<lstack> peek: empty stack"))
  (let ([v (peek s)])
    (set! (top s) (next (top s)))
    (dec! (length s))
    v))

(define-method clear! ([s <lstack>])
  "Clears the stack s. O(1)"
  (set! (top s) (make <node> :value #f :next #f))
  (set! (length s) 0))

;; <aqueue> array queue

(define-class <aqueue> ()
  ([capacity :init-keyword :capacity init-value 0 :getter capacity]
   [array :accessor array]
   [front :init-value 0 :accessor front]
   [back :init-value 0 :accessor back]
   [length :init-value 0 :accessor length]))

(define-method initialize ([q <aqueue>] _)
  "Initialize capacity of the queue q"
  (next-method)
  (set! (array q) (make-vector (capacity q))))

(define-method write-object ([q <aqueue>] p)
  "Writes the representation of the queueu to the port p"
  (let ([v (vector-copy (array q))] [f (front q)] [b (back q)])
    (cond
      [(= f b) (set! (~ v f) (cons (~ v f) 'fb))]
      [else
       (set! (~ v f) (cons (~ v f) 'f))
       (set! (~ v b) (cons (~ v b) 'b))])
    (format p "#<~a ~a as ~a>"
            (class-name (current-class-of q))
            (length q) v)))

(define-method empty? ([q <aqueue>])
  "Returns #t if the queue q is empty, otherwise #f. O(1)"
  [zero? (length q)])

(define-method enqueue! ([q <aqueue>] e)
  "Enqueues the element e at the back of the queue q. O(1)"
  (when [>= (length q) (capacity q)] (error "<aqueue> enqueue!: exceeded capacity"))
  (set! (~ (array q) (back q)) e)
  (set! (back q) (remainder (+ (back q) 1) (capacity q)))
  (inc! (length q)))

(define-method dequeue! ([q <aqueue>])
  "Dequeues the eleemnt from the front of the queue q. O(1)"
  (when [empty? q] (error "<aqueue> dequeue!: empty queue"))
  (let ([e (peek q)])
    (set! (front q) (remainder (+ (front q) 1) (capacity q)))
    (dec! (length q))
    e))

(define-method peek ([q <aqueue>])
  "Returns the eleemnt from the front of the queue q. O(1)"
  (when [empty? q] (error "<aqueue> peek: empty queue"))
  (~ (array q) (front q)))

(define-method clear! ([q <aqueue>])
  "Clears the queue q. O(1)"
  (set! (front q) 0) (set! (back q) 0) (set! (length q) 0))

;; <lqueue> linked list queue

(define-class <lqueue> ()
  ([front :accessor front]
   [back :accessor back]
   [length :init-value 0 :accessor length]))

(define-method initialize ([q <lqueue>] _)
  "initilizes the front and the back of the queue q with the null node"
  (next-method)
  (let ([n (null-node)])
    (set! (front q) n) (set! (back q) n)))

(define ((node-mark-fb l) n s)
  "Marks the front and the back nodes in the list of nodes l"
  (let ([v (value n)] [front? (eq? n (front l))] [back? (eq? n (back l))])
    (cond
      [(and front? back?) (cons (cons v 'fb) s)]
      [front? (cons (cons v 'f) s)]
      [back? (cons (cons v 'b) s)]
      [else (cons v s)])))

(define-method write-object ([q <lqueue>] p)
  "Writes the representation of the queue q to the port p"
  (format p "#<~a ~a as ~a>"
          (class-name (current-class-of p))
          (length q) (node-fold-next (node-mark-fb q) '() (front q))))

(define-method empty? ([q <lqueue>])
  "Returns #t if the queue q is empty, otherwise #f. O(1)"
  [zero? (length q)])

(define-method enqueue! ([q <lqueue>] v)
  "Enqueues the value v at the back of the queue q. O(1)"
  (let ([n (make <node> :value v :next #f)])
    (set! (next (back q)) n)
    (set! (back q) n)
    (inc! (length q))))

(define-method dequeue! ([q <lqueue>])
  "Dequeues the value from the front of the queue q. O(1)"
  (when [empty? q] (error "<lqueue> dequeue!: empty queue"))
  (let ([v (peek q)])
    (set! (front q) (next (front q)))
    (dec! (length q))
    v))

(define-method peek ([q <lqueue>])
  "Returns the value from the front of the queue q. O(1)"
  (when [empty? q] (error "<lqueue> dequeue!: empty queue"))
  (value (next (front q))))

(define-method clear! ([q <lqueue>])
  "Clears the queue q. O(1)"
  (let ([n (null-node)])
    (set! (front q) n) (set! (back q) n)
    (set! (length q) 0)))

;; <adict> array dictionary

(define-class <adict> ()
  ([capacity :init-keyword :capacity :init-value 10 :getter capacity]
   [array :accessor array]
   [length :allocation :virtual :slot-ref (lambda (d) (length (array d)))
           :getter length]))

(define-method initialize ([d <adict>] a)
  "Initializes capacity of the dictionary d"
  (next-method)
  (set! (array d) (apply make <avector> a)))

(define-method write-object ([d <adict>] p)
  "Writes the prepresentation of the dictionary d to the port p"
  (format p "#<~a as ~a>"
          (class-name (current-class-of d)) (write-object (array d) #f)))

(define-method empty? ([d <adict>])
  "Returns #t if the dictionary d is empty, otherwise #f. O(1)"
  (empty? (array d)))

(define-method insert! ([d <adict>] k v)
  "Inserts the key-value pair k v into the dictionary. O(1)"
  (append! (array d) (cons k v)))

(define-method search ([d <adict>] k :optional (c equal?))
  "Searches the dictionary d for the key k. O(n)"
  (let ([a (array d)] [l (length d)])
    (start! a)
    (do ([i 0 (+ i 1)])
        ((or [>= i l] [c (car (value a)) k]) (if [>= i l] #f (cdr (value a))))
      (next! a))))

(define-method remove! ([d <adict>] k :optional (c equal?))
  "Removes the key-value pair with the key k from the dictionary d. O(n)"
  (let ([a (array d)] [l (length d)])
    (start! a)
    (do ([i 0 (+ i 1)])
        ((or [>= i l] [c (car (value a)) k])
         (if [>= i l] #f (let ([v (cdr (value a))]) (remove! a) v)))
      (next! a))))

(define-method clear! ([d <adict>])
  "Clears the dicutionary d. O(1)"
  (clear! (array d)))

;; <ldict> linked list dictionary

(define-class <ldict> ()
  ([llist :init-form (make <llist>) :accessor llist]
   [length :allocation :virtual :slot-ref (lambda (d) (length (llist d)))
           :getter length]))

(define-method write-object ([d <ldict>] p)
  "Writes the representation of the diction d to the port p"
  (format p "#<~a> as ~a"
          (class-name (current-class-of d)) (write-object (llist d) #f)))

(define-method empty? ([d <ldict>])
  "Returns #t if the dictionary d is empty, otherwise #f. O(1)"
  (empty? (llist d)))

(define-method insert! ([d <ldict>] k v)
  "Inserts the key-value pair k v into the dictionary. O(1)"
  (append! (llist d) (cons k v)))

(define-method search ([d <ldict>] k :optional (c equal?))
  "Searches the dictionary d for the key k. O(n)"
  (let ([m (llist d)] [l (length d)])
    (start! m)
    (do ([i 0 (+ i 1)])
        ((or [>= i l] (c (car (value m)) k)) (if [>= i l] #f (cdr (value m))))
      (next! m))))

(define-method remove! ([d <ldict>] k :optional (c equal?))
  "Removes the key-value pair with the key k from the dictionary d. O(n)"
  (let ([m (llist d)] [l (length d)])
    (start! m)
    (do ([i 0 (+ i 1)])
        ((or [>= i l] (c (car (value m)) k))
         (if [>= i l] #f (let ([v (cdr (value m))]) (remove! m) v)))
      (next! m))))

(define-method clear! ([d <ldict>])
  "Clears the dicutionary d. O(1)"
  (clear! (llist d)))

;; Testing

(define (lv-test-insert! x)
  (print x)
  (insert! x 1) (print x)
  (insert! x 3) (print x)
  (end! x) (print x)
  (insert! x 2) (print x)
  (start! x) (print x)
  (insert! x 5) (print x)
  (position! x 1) (print x)
  (insert! x 4) (print x))

(define (lv-test-append! x)
  (print x)
  (append! x 1) (print x)
  (append! x 2) (print x))

(define (lv-test-remove! x)
  (print x)
  (insert! x 1) (insert! x 2) (insert! x 3) (insert! x 4) (insert! x 5) (print x)
  #?=(remove! x) (print x)
  (end! x) (print x)
  #?=(remove! x) (print x)
  (start! x) (print x)
  #?=(remove! x) (print x)
  (position! x 1) (print x)
  #?=(remove! x) (print x))

(define (lv-test-position! x)
  (print x)
  (insert! x 1) (insert! x 2) (insert! x 3) (insert! x 4) (insert! x 5) (print x)
  (end! x) (print x)
  (start! x) (print x)
  (position! x 2) (print x)
  (next! x) (print x)
  (previous! x) (print x))

(define (lv-test-update! x)
  (print x)
  (insert! x 1) (print x)
  (update! x 10) (print x)
  #?=(value x)
  #?=(empty? x)
  (clear! x)
  #?=(empty? x))

(define (st_test! x)
  (print x)
  (push! x 1) (print x)
  (push! x 2) (print x)
  (push! x 3) (print x)
  #?=(peek x) (print x)
  #?=(pop! x) (print x)
  #?=(pop! x) (print x)
  (clear! x) (print x))

(define (qu_test! x)
  (print x)
  (enqueue! x 1) (print x)
  (enqueue! x 2) (print x)
  (enqueue! x 3) (print x)
  #?=(peek x) (print x)
  #?=(dequeue! x) (print x)
  #?=(dequeue! x) (print x)
  #?=(dequeue! x) (print x)
  (enqueue! x 4) (print x)
  (enqueue! x 5) (print x)
  (enqueue! x 6) (print x)
  #?=(dequeue! x) (print x)
  #?=(dequeue! x) (print x)
  #?=(dequeue! x) (print x)
  (clear! x) (print x))

(define (dc_test! x)
  (print x)
  (insert! x 'a 1) (print x)
  (insert! x 'b 2) (print x)
  (insert! x 'c 3) (print x)
  (insert! x 'd 4) (print x)
  #?=(search x 'b)
  #?=(search x 'd eq?)
  #?=(search x 'x eq?)
  #?=(remove! x 'a) (print x)
  #?=(remove! x 'd eq?) (print x)
  #?=(remove! x 'x eq?) (print x)
  (clear! x) (print x))

;; (let ([x (make <avector> :capacity 5)])
;; (let ([x (make <llist>)])
;; (let ([x (make <dlist>)])
  ;; (lv-test-insert! x)
  ;; (lv-test-append! x)
  ;; (lv-test-remove! x)
  ;; (lv-test-position! x)
  ;; (lv-test-update! x))

;; (let ([x (make <astack> :capacity 5)])
;; (let ([x (make <lstack> :capacity 5)])
;;   (st_test! x))

(define (factorial n)
  "Tail recursive factorial"
  (let factorial* ([i n] [r 1])
    (cond [(< i 2) r] [else (factorial* (- i 1) (* r i))])))

;; #?=(factorial 0)
;; #?=(factorial 1)
;; #?=(factorial 4)

(define (factorial2 n)
  "Iterative factorial with loops and an explicit stack"
  (do ([i n (- i 1)] [s (list 1) (cons i s)])
      ([< i 2]
       (do ([r (car s) (* r (car s))] [s (cdr s) (cdr s)]) ([null? s] r)))))

;; #?=(factorial2 0)
;; #?=(factorial2 1)
;; #?=(factorial2 4)

;; (let ([x (make <aqueue> :capacity 5)])
;; (let ([x (make <lqueue> :capacity 5)])
;;   (qu_test! x))

;; (let ([x (make <adict> :capacity 5)])
;; (let ([x (make <ldict> :capacity 5)])
;;   (dc_test! x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ([p 100] [c 5] [i 0.05] [n 10] [t 0])
  (do ([j 1 (+ j 1)]) ([> j n])
    (let ([cf (/ (if [= j n] (+ c p) c) (expt (+ 1 i) j))])
      (set! t (+ t cf))
      (format #t "~2d: ~12,3f ~12,3f\n" j cf t))))

;; (let* ([pv 145000]
;;        [i 0.005]
;;        [n 17]
;;        [k (fold (lambda (j s) (+ (/ (expt (+ 1 i) j)) s)) 0 (iota n 1))]
;;        [cf (/ pv k 12)]
;;        [t (* cf n 12)])
;;   (format #t "PV = ~12,3f\nCF = ~12,3f\n T = ~12,3f\n" pv cf t))
