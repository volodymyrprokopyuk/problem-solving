(define-module
  (data-structure)
  #:replace
  (make-stack peek)
  #:export
  (stack-null? push pop
               make-queue queue-null? enqueue dequeue front
               make-queue2 queue2-null? enqueue2 dequeue2 front2))

(use-modules
 (ice-9 receive)
 ((ice-9 pretty-print)
  #:select ((pretty-print . pp))))

;; Stack (push (cons top), pop (car top) linked list)

(define (make-stack)
  "Creates an empty stack (LIFO)"
  '())

(define (stack-null? s)
  "Returns #t if the stack is empty"
  (null? s))

(define (push e s)
  "Inserts a new element into the stack and returns the new stack"
  (cons e s))

(define (pop s)
  "Removes the top element from the stack and returns the new stack"
  (when (stack-null? s) (error "stack: empty stack"))
  (values (car s) (cdr s)))

(define (peek s)
  "Returns the top element from the stack without removing the element"
  (when (stack-null? s) (error "stack: empty stack"))
  (car s))

;; (let* ([s (make-stack)]
;;        [s (push 'a s)]
;;        [s (push 'b s)])
;;   (pp (stack-null? s))
;;   (pp (peek s))
;;   (receive (e s) (pop s)
;;     (pp e)
;;     (pp (stack-null? s))
;;     (receive (e s) (pop s)
;;       (pp e)
;;       (pp (stack-null? s))
;;       (let ([s (push 'c s)])
;;         (receive (e s) (pop s)
;;           (pp e)
;;           (pp (stack-null? s)))))))

;; Queue (dequeue (car front) linked list enqueue (set-cdr! back))

(define (make-queue)
  "Creates an empty queue (FIFO)"
  (let ([end (cons 'end '())])
    ;; front back
    (cons end end)))

(define (queue-null? q)
  "Returns #t if the queue is empty"
  (equal? (car q) (cdr q)))

(define (enqueue e q)
  "Inserts the element at the back of the queue and returns the new queue"
  (let ([end (cons 'end '())]
        [back (cdr q)])
    (set-car! back e)
    (set-cdr! back end)
    (cons (car q) end)))

(define (dequeue q)
  "Removes the element from the front of the queue and returns the new queue"
  (when (queue-null? q) (error "queue: empty queue"))
  (let ([front (car q)])
    (values (car front) (cons (cdr front) (cdr q)))))

(define (front q)
  "Returns the element from the front of the queue without removing the element"
  (when (queue-null? q) (error "queue: empty queue"))
  (let ([front (car q)])
    (car front)))

;; (let* ([q (make-queue)]
;;        [q (enqueue 'a q)]
;;        [q (enqueue 'b q)])
;;   (pp (queue-null? q))
;;   (pp (front q))
;;   (receive (e q) (dequeue q)
;;     (pp e)
;;     (pp (queue-null? q))
;;     (receive (e q) (dequeue q)
;;       (pp e)
;;       (pp (queue-null? q))
;;       (let ([q (enqueue 'c q)])
;;         (receive (e q) (dequeue q)
;;           (pp e)
;;           (pp (queue-null? q)))))))

;; Queue (enqueue (push back) back->front dequeue (pop front))

(define (make-queue2)
  "Creates an empty queue (FIFO)"
  ;; front back
  (cons (make-stack) (make-stack)))

(define (queue2-null? q)
  "Returns #t if the queue is empty"
  (and (stack-null? (car q)) (stack-null? (cdr q))))

(define (enqueue2 e q)
  "Inserts the element at the back of the queue and returns the new queue"
  (let ([back (cdr q)])
    (cons (car q) (push e back))))

(define (back->front q)
  "Moves elements from the back stack to the front stack of the queue"
  " and returs the new queue"
  (let move* ([front (car q)]
              [back (cdr q)])
    (if [stack-null? back] (cons front back)
        (receive (e back) (pop back)
          (move* (push e front) back)))))

(define (dequeue2 q)
  "Removes the element from the top of the queue and returns the new queue"
  (when (queue2-null? q) (error "queue2: empty queue"))
  (let* ([q (if [stack-null? (car q)] (back->front q) q)]
         [front (car q)])
    (receive (e front) (pop front)
      (values e (cons front (cdr q))))))

(define (front2 q)
  "Returns the element from the front of the queue without removing the element"
  (when (queue2-null? q) (error "queue2: empty queue"))
  (let* ([q (if [stack-null? (car q)] (back->front q) q)]
         [front (car q)])
    (peek front)))

;; (let* ([q (make-queue2)]
;;        [q (enqueue2 'a q)]
;;        [q (enqueue2 'b q)])
;;   (pp (queue2-null? q))
;;   (pp (front2 q))
;;   (receive (e q) (dequeue2 q)
;;     (pp e)
;;     (pp (queue2-null? q))
;;     (receive (e q) (dequeue2 q)
;;       (pp e)
;;       (pp (queue2-null? q))
;;       (let ([q (enqueue2 'c q)])
;;         (receive (e q) (dequeue2 q)
;;           (pp e)
;;           (pp (queue2-null? q)))))))
