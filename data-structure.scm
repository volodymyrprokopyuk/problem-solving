(define-module
  (data-structure)
  #:export
  (make-stack stack-null? push pop peek
              make-queue queue-null? enqueue dequeue front))

(use-modules
 (ice-9 receive)
 ((ice-9 pretty-print)
  #:select ((pretty-print . pp))))

;; Stack

(define (make-stack)
  "Creates an empty stack (LIFO)"
  '())

(define (stack-null? s)
  "Returns #t if the stack is empty"
  (null? s))

(define (push s e)
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
;;        [s (push s 'a)]
;;        [s (push s 'b)])
;;   (pp (stack-null? s))
;;   (pp (peek s))
;;   (receive (e s) (pop s)
;;     (pp e)
;;     (pp (stack-null? s))
;;     (receive (e s) (pop s)
;;       (pp e)
;;       (pp (stack-null? s)))))

;; Queue

(define (make-queue)
  "Creates an empty queue (FIFO)"
  (let ([end (cons 'end '())])
    (cons end end)))

(define (queue-null? q)
  "Returns #t if the queue is empty"
  (equal? (car q) (cdr q)))

(define (enqueue q e)
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
;;        [q (enqueue q 'a)]
;;        [q (enqueue q 'b)])
;;   (pp (queue-null? q))
;;   (pp (front q))
;;   (receive (e q) (dequeue q)
;;     (pp e)
;;     (pp (queue-null? q))
;;     (receive (e q) (dequeue q)
;;       (pp e)
;;       (pp (queue-null? q)))))
