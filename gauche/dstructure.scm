(define-module dstructure)

(select-module dstructure)

(define-class <avector> ()
  ((capacity :init-keyword :capacity :init-value 10 :getter capacity)
   (length :init-value 0 :accessor length)
   (position :init-value 0 :accessor position)
   (array :accessor array)
   (value :allocation :virtual :slot-ref (lambda (v) (~ (array v) (position v)))
          :getter value)))

(define-method initialize ([v <avector>] _)
  "Initializes the vector v with the storage capacity"
  (next-method)
  (set! (array v) (make-vector (capacity v))))

(define-method write-object ([v <avector>] p)
  "Writes the vector v representation to the porte p"
  (format p "#<~a ~a of ~a at ~a as ~a>"
          (class-name (current-class-of v))
          (length v) (capacity v) (position v) (array v)))

(define-method empty? ([v <avector>])
  "Returns #t if the vector v is empty, #f otherwise. O(1)"
  [zero? (length v)])

(define-method next! ([v <avector>])
  "Moves the current position to the next element. O(1)"
  (when [< (position v) (length v)] (inc! (position v))))

(define-method previous! ([v <avector>])
  "Moves the current position to the previous element. O(1)"
  (when [> (position v) 0] (dec! (position v))))

(define-method start! ([v <avector>])
  "Moves the current position to the start of the vector. O(1)"
  (set! (position v) 0))

(define-method end! ([v <avector>])
  "Moves the current position to the end of the vector. O(1)"
  (set! (position v) (- (length v) 1)))

(define-method insert! ([v <avector>] e)
  "Inserts the element e into the vector v at the current position. O(n)"
  (when [= (length v) (capacity v)] (error "<avector> insert!: exceeded capacity"))
  (let ([l (length v)] [p (position v)] [a (array v)])
    (do ([i (- l 1) (- i 1)]) ([< i p]) (set! (~ a (+ i 1)) (~ a i)))
    (set! (~ a p) e) (inc! (length v))))

(define-method update! ([v <avector>] e)
  "Updates the element at the current position of the vector v with e. O(1)"
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

(let ([v (make <avector> :capacity 5)])
  #?=(empty? v)
  (insert! v 1)
  (insert! v 2)
  (insert! v 3)
  (insert! v 4)
  (insert! v 5)
  (print v)
  #?=(empty? v)
  (end! v)
  #?=(value v)
  (start! v)
  #?=(value v)
  (update! v 10)
  (print v)
  (end! v)
  (update! v 20)
  (print v)
  (set! (position v) 2)
  #?=(value v)
  #?=(remove! v)
  (print v)
  (end! v)
  (insert! v 22)
  (print v)
  (end! v)
  #?=(remove! v)
  (start! v)
  (insert! v 11)
  (print v)
  (clear! v)
  (insert! v 33)
  (print v))
