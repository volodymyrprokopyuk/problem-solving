(define-module
  (data-structure)
  #:replace
  (make-stack)
  #:export (make-clist cl-empty? cl-insert! cl-remove! cl-head cl-shift cl-content
                       make-clist-obj make-stack st-empty? st-push st-pop st-peek
                       st-content))

(use-modules
 (ice-9 receive)
 ((ice-9 pretty-print)
  #:select ((pretty-print . pp))))

;; Circular list

(define (make-clist)
  "Creates an empty cicrular list"
  '())

(define (cl-empty? marker)
  "Returns #t if the circular list marker is empty"
  (null? marker))

(define (cl-insert! e marker)
  "Inserts the element e into the circular list marker"
  (cond
    [(cl-empty? marker)
     (let ([marker (cons e marker)])
       (set-cdr! marker marker) marker)]
    [else (set-cdr! marker (cons e (cdr marker))) marker]))

(define (cl-remove! marker)
  "Removes the head of the circular list marker"
  (when [cl-empty? marker] (error "clist: empty circular list"))
  (let ([head (cdr marker)])
    (cond
      [(eq? head marker) (values (car head) '())]
      [else (set-cdr! marker (cdr head)) (values (car head) marker)])))

(define (cl-head marker)
  "Returns the head of the circular list marker"
  (when [cl-empty? marker] (error "clist: empty circular list"))
  (cadr marker))

(define (cl-shift marker)
  "Shifts the marker of the circular list marker to the head of the circular list"
  (when [cl-empty? marker] (error "clist: empty circular list"))
  (cdr marker))

(define (cl-content marker)
  "Returns the content of the circular list marker"
  (if [cl-empty? marker] marker (cdr marker)))

;; (let* ([cl (make-clist)]
;;        [cl (cl-insert! 'a cl)]
;;        [cl (cl-insert! 'b cl)])
;;   (pp (cl-empty? cl))
;;   (pp (cl-content cl))
;;   (pp (cl-head cl))
;;   (receive (e cl) (cl-remove! cl)
;;     (pp e)
;;     (pp (cl-head cl))
;;     (receive (e cl) (cl-remove! cl)
;;       (pp e)
;;       (pp (cl-empty? cl)))))

;; (let* ([cl (make-clist)]
;;        [cl (cl-insert! 'a cl)]
;;        [cl (cl-insert! 'b cl)]
;;        [cl (cl-insert! 'c cl)])
;;   (do ([cl cl (cl-shift cl)] [i 0 (1+ i)]) ([> i 3]) (pp (cl-content cl))))

;; Circular list object

(define (make-clist-obj)
  "Returns a circular list object"
  (let ([marker '()])
    (lambda (method . args)
      (case method
        [(type) "clist"]
        [(empty?) (null? marker)]
        [(insert!)
         (cond
           [(null? marker)
            (set! marker (cons (car args) marker)) (set-cdr! marker marker)]
           [else (set-cdr! marker (cons (car args) (cdr marker)))])]
        [(remove!)
         (when [null? marker] (error "clist: empty circular list"))
         (let ([head (cdr marker)])
           (cond
             [(eq? head marker) (set! marker '())]
             [else (set-cdr! marker (cdr head))])
           (car head))]
        [(head)
         (when [null? marker] (error "clist: empty circular list"))
         (cadr marker)]
        [(shift!)
         (when [null? marker] (error "clist: empty circular list"))
         (set! marker (cdr marker))]
        [(content) (if [null? marker] marker (cdr marker))]
        [else (error "clist: not supported method:" method)]))))

;; (let ([cl (make-clist-obj)])
;;   (pp (cl 'empty?))
;;   (pp (cl 'content))
;;   (cl 'insert! 'a)
;;   (cl 'insert! 'b)
;;   (pp (cl 'empty?))
;;   (pp (cl 'content))
;;   (pp (cl 'head))
;;   (pp (cl 'remove!))
;;   (pp (cl 'head))
;;   (pp (cl 'remove!))
;;   (pp (cl 'empty?))
;;   (pp (cl 'content))
;;   ;; shift!
;;   (cl 'insert! 'a)
;;   (cl 'insert! 'b)
;;   (cl 'insert! 'c)
;;   (pp (cl 'content))
;;   (cl 'shift!)
;;   (pp (cl 'content))
;;   (cl 'shift!)
;;   (pp (cl 'content))
;;   (cl 'shift!)
;;   (pp (cl 'content)))

;; Stack

(define (make-stack)
  "Creates an empty stack (LIFO)"
  '())

(define (st-empty? st)
  "Returns #t if the stack st is empty"
  (null? st))

(define (st-push e st)
  "Inserts a new element e into the stack st and returns the new stack"
  (cons e st))

(define (st-pop st)
  "Removes the top element from the stack st and returns the new stack"
  (when (st-empty? st) (error "stack: empty stack"))
  (values (car st) (cdr st)))

(define (st-peek st)
  "Returns the top element from the stack st without removing the element"
  (when (st-empty? st) (error "stack: empty stack"))
  (car st))

(define (st-content st)
  "Returns the content of the stack st"
  st)

;; (let* ([st (make-stack)]
;;        [st (st-push 'a st)]
;;        [st (st-push 'b st)])
;;   (pp (st-empty? st))
;;   (pp (st-content st))
;;   (pp (st-peek st))
;;   (receive (e st) (st-pop st)
;;     (pp e)
;;     (pp (st-empty? st))
;;     (pp (st-peek st))
;;     (receive (e st) (st-pop st)
;;       (pp e)
;;       (pp (st-empty? st))
;;       (let ([st (st-push 'c st)])
;;         (receive (e st) (st-pop st)
;;           (pp e)
;;           (pp (st-empty? st)))))))

;; Stack object

(define (make-stack-obj)
  "Returns a stack object"
  (let ([st '()])
    (lambda (method . args)
      (case method
        [(type) "stack"]
        [(empty?) (null? st)]
        [(push!) (set! st (cons (car args) st))]
        [(pop!)
         (when [null? st] (error "stack: empty stack"))
         (let ([e (car st)])
           (set! st (cdr st)) e)]
        [(peek)
         (when [null? st] (error "stack: empty stack"))
         (car st)]
        [else (error "stack: not supported method:" method)]))))

;; (let ([stack (make-stack-obj)])
;;   (stack 'push! 'a)
;;   (stack 'push! 'b)
;;   (pp (stack 'empty?))
;;   (pp (stack 'peek))
;;   (pp (stack 'pop!))
;;   (pp (stack 'empty?))
;;   (pp (stack 'peek))
;;   (pp (stack 'pop!))
;;   (pp (stack 'empty?))
;;   (stack 'push! 'c)
;;   (pp (stack 'pop!))
;;   (pp (stack 'empty?)))

;; Queue

(define (make-queue)
  "Creates an empty queue (FIFO)"
  ;; #f is the postion to set-car! new element
  (let* ([queue (cons #f '())] [back queue])
    (cons queue back)))

(define (queue-null? q)
  "Returns #t if the queue is empty"
  (eq? (car q) (cdr q)))

(define (enqueue e q)
  "Inserts the element at the back of the queue and returns the new queue"
  (let ([back (cdr q)] [next (cons #f '())])
    (set-car! back e) (set-cdr! back next) (cons (car q) next)))

(define (dequeue q)
  "Removes the element from the front of the queue and returns the new queue"
  (when (queue-null? q) (error "queue: empty queue"))
  (let ([queue (car q)])
    (values (car queue) (cons (cdr queue) (cdr q)))))

(define (front q)
  "Returns the element from the front of the queue without removing the element"
  (when (queue-null? q) (error "queue: empty queue"))
  (let ([queue (car q)])
    (car queue)))

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

;; Queue object

(define (make-queue2)
  "Returns a queue"
  ;; #f is the postion to set-car! new element
  (let* ([queue (cons #f '())] [back queue])
    (lambda (m . args)
      (case m
        [(type) "queue"]
        [(queue-null?) (eq? queue back)]
        [(enqueue!)
         (let ([next (cons #f '())])
           (set-car! back (car args)) (set-cdr! back next) (set! back next))]
        [(dequeue!)
         (when [eq? queue back] (error "queue: empty queue"))
         (let ([e (car queue)])
           (set! queue (cdr queue)) e)]
        [(front)
         (when [eq? queue back] (error "queue: empty queue"))
         (car queue)]
        [(content) queue]
        [else (error "queue: not supported method:" m)]))))

;; (let ([queue (make-queue2)])
;;   (queue 'enqueue! 'a)
;;   (queue 'enqueue! 'b)
;;   (pp (queue 'content))
;;   (pp (queue 'queue-null?))
;;   (pp (queue 'front))
;;   (pp (queue 'dequeue!))
;;   (pp (queue 'queue-null?))
;;   (pp (queue 'dequeue!))
;;   (pp (queue 'queue-null?))
;;   (queue 'enqueue! 'c)
;;   (pp (queue 'dequeue!))
;;   (pp (queue 'queue-null?)))

;; Queue (enqueue (push back) back->front dequeue (pop front))

(define (make-queue3)
  "Creates an empty queue (FIFO)"
  ;; front back
  (cons (make-stack) (make-stack)))

(define (queue3-null? q)
  "Returns #t if the queue is empty"
  (and (stack-null? (car q)) (stack-null? (cdr q))))

(define (enqueue3 e q)
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

(define (dequeue3 q)
  "Removes the element from the top of the queue and returns the new queue"
  (when (queue3-null? q) (error "queue3: empty queue"))
  (let* ([q (if [stack-null? (car q)] (back->front q) q)]
         [front (car q)])
    (receive (e front) (pop front)
      (values e (cons front (cdr q))))))

(define (front3 q)
  "Returns the element from the front of the queue without removing the element"
  (when (queue3-null? q) (error "queue3: empty queue"))
  (let* ([q (if [stack-null? (car q)] (back->front q) q)]
         [front (car q)])
    (peek front)))

;; (let* ([q (make-queue3)]
;;        [q (enqueue3 'a q)]
;;        [q (enqueue3 'b q)])
;;   (pp (queue3-null? q))
;;   (pp (front3 q))
;;   (receive (e q) (dequeue3 q)
;;     (pp e)
;;     (pp (queue3-null? q))
;;     (receive (e q) (dequeue3 q)
;;       (pp e)
;;       (pp (queue3-null? q))
;;       (let ([q (enqueue3 'c q)])
;;         (receive (e q) (dequeue3 q)
;;           (pp e)
;;           (pp (queue3-null? q)))))))
