(define-module
  (data-structure)
  #:replace
  (make-stack)
  #:export (make-clist cl-empty? cl-insert! cl-remove! cl-head cl-shift cl-content
                       make-clist-obj make-stack st-empty? st-push st-pop st-peek
                       st-content make-stack-obj make-queue qu-empty? qu-enqueue
                       qu-dequeue qu-front qu-content make-queue-obj))

(use-modules
 (ice-9 receive)
 (ice-9 match)
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
  (if [cl-empty? marker] marker (cdr marker)))

(define (cl-content marker)
  "Returns the content of the circular list marker"
  (if [cl-empty? marker] marker (cdr marker)))

(define (cl-reverse marker)
  "Reverses the circular list marker"
  (let reverse* ([cl marker] [r (make-clist)])
    (cond
      [(cl-empty? cl) (cl-shift r)]
      [else
       (receive (e cl) (cl-remove! cl)
         (reverse* cl (cl-insert! e r)))])))

;; (let* ([cl (make-clist)]
;;        [cl (cl-insert! 'a cl)]
;;        [cl (cl-insert! 'b cl)])
;;   (pp (cl-empty? cl))
;;   (pp (cl-content cl))
;;   (pp (cl-head cl))
;;   (receive (e cl) (cl-remove! cl)
;;     (pp e)
;;     (pp (cl-empty? cl))
;;     (pp (cl-head cl))
;;     (receive (e cl) (cl-remove! cl)
;;       (pp e)
;;       (pp (cl-empty? cl)))))

;; (let* ([cl (make-clist)]
;;        [cl (cl-insert! 'a cl)]
;;        [cl (cl-insert! 'b cl)]
;;        [cl (cl-insert! 'c cl)])
;;   (do ([cl cl (cl-shift cl)] [i 0 (1+ i)]) ([> i 3]) (pp (cl-content cl))))

;; (let* ([cl (make-clist)]
;;        [cl (cl-insert! 'a cl)]
;;        [cl (cl-insert! 'b cl)]
;;        [cl (cl-insert! 'c cl)]
;;        [cl (cl-insert! 'd cl)])
;;   (pp (cl-content cl))
;;   (pp (cl-reverse cl)))

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
        [(shift!) (unless [null? marker] (set! marker (cdr marker)))]
        [(content) (if [null? marker] marker (cdr marker))]
        [(reverse!)
         (cond
           [(null? marker) marker]
           [else
            (let reverse* ([a marker] [b (cdr marker)])
              (cond
                [(eq? b marker) (set! marker (cdr marker)) (set-cdr! b a)]
                [else
                 (let ([c (cdr b)])
                   (set-cdr! b a) (reverse* b c))]))])]
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
;;   ;; reverse!
;;   (cl 'reverse!)
;;   (pp (cl 'content))
;;   (cl 'insert! 'a)
;;   (cl 'reverse!)
;;   (pp (cl 'content))
;;   ;; shift!
;;   (cl 'insert! 'b)
;;   (cl 'insert! 'c)
;;   (pp (cl 'content))
;;   (cl 'shift!)
;;   (pp (cl 'content))
;;   (cl 'shift!)
;;   (pp (cl 'content))
;;   (cl 'shift!)
;;   (pp (cl 'content))
;;   ;; reverse!
;;   (cl 'insert! 'd)
;;   (pp (cl 'content))
;;   (cl 'reverse!)
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
        [(content) st]
        [else (error "stack: not supported method:" method)]))))

;; (let ([st (make-stack-obj)])
;;   (st 'push! 'a)
;;   (st 'push! 'b)
;;   (pp (st 'empty?))
;;   (pp (st 'content))
;;   (pp (st 'peek))
;;   (pp (st 'pop!))
;;   (pp (st 'empty?))
;;   (pp (st 'peek))
;;   (pp (st 'pop!))
;;   (pp (st 'empty?))
;;   (st 'push! 'c)
;;   (pp (st 'pop!))
;;   (pp (st 'empty?)))

;; Stack (on circular list)

(define (make-stack2-obj)
  "Returns a stack object"
  (let ([cl (make-clist-obj)])
    (lambda (method . args)
      (case method
        [(type) "stack"]
        [(empty?) (cl 'empty?)]
        [(push!) (cl 'insert! (car args))]
        [(pop!) (cl 'remove!)]
        [(peek) (cl 'head)]
        [(content) (cl 'content)]
        [else (error "stack: not supported method:" method)]))))

;; (let ([st (make-stack2-obj)])
;;   (st 'push! 'a)
;;   (st 'push! 'b)
;;   (pp (st 'empty?))
;;   (pp (st 'content))
;;   (pp (st 'peek))
;;   (pp (st 'pop!))
;;   (pp (st 'empty?))
;;   (pp (st 'peek))
;;   (pp (st 'pop!))
;;   (pp (st 'empty?))
;;   (st 'push! 'c)
;;   (pp (st 'pop!))
;;   (pp (st 'empty?)))

;; Queue

(define (make-queue)
  "Creates an empty queue (FIFO)"
  ;; #f is the postion to set-car! new element
  (let* ([queue (cons #f '())] [back queue])
    (cons queue back)))

(define (qu-empty? qu)
  "Returns #t if the queue qu is empty"
  (eq? (car qu) (cdr qu)))

(define (qu-enqueue e qu)
  "Inserts the element e at the back of the queue qu and returns the new queue"
  (let ([back (cdr qu)] [next (cons #f '())])
    (set-car! back e) (set-cdr! back next) (cons (car qu) next)))

(define (qu-dequeue qu)
  "Removes the element from the front of the queue qu and returns the new queue"
  (when (qu-empty? qu) (error "queue: empty queue"))
  (let ([queue (car qu)])
    (values (car queue) (cons (cdr queue) (cdr qu)))))

(define (qu-front qu)
  "Returns the element from the front of the queue qu without removing the element"
  (when (qu-empty? qu) (error "queue: empty queue"))
  (caar qu))

(define (qu-content qu)
  "Returns the content of the queue qu"
  (car qu))

;; (let* ([qu (make-queue)]
;;        [qu (qu-enqueue 'a qu)]
;;        [qu (qu-enqueue 'b qu)])
;;   (pp (qu-empty? qu))
;;   (pp (qu-content qu))
;;   (pp (qu-front qu))
;;   (receive (e qu) (qu-dequeue qu)
;;     (pp e)
;;     (pp (qu-empty? qu))
;;     (pp (qu-front qu))
;;     (receive (e qu) (qu-dequeue qu)
;;       (pp e)
;;       (pp (qu-empty? qu))
;;       (let ([qu (qu-enqueue 'c qu)])
;;         (receive (e qu) (qu-dequeue qu)
;;           (pp e)
;;           (pp (qu-empty? qu)))))))

;; Queue object

(define (make-queue-obj)
  "Returns a queue object"
  ;; #f is the postion to set-car! new element
  (let* ([queue (cons #f '())] [back queue])
    (lambda (method . args)
      (case method
        [(type) "queue"]
        [(empty?) (eq? queue back)]
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
        [else (error "queue: not supported method:" method)]))))

;; (let ([qu (make-queue-obj)])
;;   (qu 'enqueue! 'a)
;;   (qu 'enqueue! 'b)
;;   (pp (qu 'empty?))
;;   (pp (qu 'content))
;;   (pp (qu 'front))
;;   (pp (qu 'dequeue!))
;;   (pp (qu 'empty?))
;;   (pp (qu 'front))
;;   (pp (qu 'dequeue!))
;;   (pp (qu 'empty?))
;;   (qu 'enqueue! 'c)
;;   (pp (qu 'dequeue!))
;;   (pp (qu 'empty?)))

;; Queue (on circular list)

(define (make-queue2-obj)
  "Returns a queue object"
  (let ([cl (make-clist-obj)])
    (lambda (method . args)
      (case method
        [(type) "queue"]
        [(empty?) (cl 'empty?)]
        [(enqueue!) (cl 'insert! (car args)) (cl 'shift!)]
        [(dequeue!) (cl 'remove!)]
        [(front) (cl 'head)]
        [(content) (cl 'content)]
        [else (error "queue: not supported method:" method)]))))

;; (let ([qu (make-queue2-obj)])
;;   (qu 'enqueue! 'a)
;;   (qu 'enqueue! 'b)
;;   (pp (qu 'empty?))
;;   (pp (qu 'content))
;;   (pp (qu 'front))
;;   (pp (qu 'dequeue!))
;;   (pp (qu 'empty?))
;;   (pp (qu 'front))
;;   (pp (qu 'dequeue!))
;;   (pp (qu 'empty?))
;;   (qu 'enqueue! 'c)
;;   (pp (qu 'dequeue!))
;;   (pp (qu 'empty?)))

;; Queue (on two stacks)

(define (make-queue3)
  "Creates an empty queue (FIFO)"
  ;; front back
  (cons (make-stack) (make-stack)))

(define (qu3-empty? qu)
  "Returns #t if the queue qu is empty"
  (and (st-empty? (car qu)) (st-empty? (cdr qu))))

(define (qu3-enqueue e qu)
  "Inserts the element e at the back of the queue qu and returns the new queue"
  (let ([back (cdr qu)])
    (cons (car qu) (st-push e back))))

(define (qu3-back->front qu)
  "Moves elements from the back stack to the front stack of the queue"
  " and returs the new queue"
  (let move* ([front (car qu)] [back (cdr qu)])
    (cond
      [(st-empty? back) (cons front back)]
      [else
       (receive (e back) (st-pop back)
         (move* (st-push e front) back))])))

(define (qu3-dequeue qu)
  "Removes the element from the top of the queue qu and returns the new queue"
  (when (qu3-empty? qu) (error "queue: empty queue"))
  (let* ([qu (if [st-empty? (car qu)] (qu3-back->front qu) qu)]
         [front (car qu)])
    (receive (e front) (st-pop front)
      (values e (cons front (cdr qu))))))

(define (qu3-front qu)
  "Returns the element from the front of the queue qu without removing the element"
  (when (qu3-empty? qu) (error "queue: empty queue"))
  (let* ([qu (if [st-empty? (car qu)] (qu3-back->front qu) qu)]
         [front (car qu)])
    (st-peek front)))

(define (qu3-content qu)
  "Returns the content of the queue qu"
  (let ([qu (qu3-back->front qu)])
    (st-content (car qu))))

;; (let* ([qu (make-queue3)]
;;        [qu (qu3-enqueue 'a qu)]
;;        [qu (qu3-enqueue 'b qu)])
;;   (pp (qu3-empty? qu))
;;   (pp (qu3-content qu))
;;   (pp (qu3-front qu))
;;   (receive (e qu) (qu3-dequeue qu)
;;     (pp e)
;;     (pp (qu3-empty? qu))
;;     (pp (qu3-front qu))
;;     (receive (e qu) (qu3-dequeue qu)
;;       (pp e)
;;       (pp (qu3-empty? qu))
;;       (let ([qu (qu3-enqueue 'c qu)])
;;         (receive (e qu) (qu3-dequeue qu)
;;           (pp e)
;;           (pp (qu3-empty? qu)))))))

;; Hash

;; TODO

;; Hash object

(define (make-hash-obj hash size)
  "Returns a hash table object with the hash function hash and of size size"
  ;; Every vector element is a backet/slot = alist
  (let ([v (make-vector size '())])
    (lambda (method . args)
      (case method
        [(type) "hash"]
        [(insert!)
         (match-let* ([(key value) args] [i (hash key)] [al (vector-ref v i)])
           (vector-set! v i (assoc-set! al key value)))]
        [(remove!)
         (match-let* ([(key) args] [i (hash key)] [al (vector-ref v i)])
           (vector-set! v i (assoc-remove! al key)))]
        [(lookup)
         (match-let* ([(key success failure) args] [i (hash key)] [al (vector-ref v i)])
           (if [null? al] (failure)
               (let ([p (assoc key al)])
                 (if p (success (cdr p)) (failure)))))]
        [(content)
         (let ([c '()])
           (do ([i 0 (1+ i)]) ([= i size] c)
             (let ([al (vector-ref v i)])
               (unless [null? al] (set! c (append al c))))))]
        [else (error "hash: not supported method:" method)]))))

;; (let ([ha (make-hash-obj (lambda (k) (remainder k 100)) 100)])
;;   (ha 'insert! 1 'a)
;;   (ha 'insert! 2 'b)
;;   (ha 'insert! 3 'c)
;;   (ha 'insert! 10 'A)
;;   (ha 'insert! 20 'B)
;;   (ha 'insert! 30 'C)
;;   (ha 'insert! 100 'Z)
;;   (pp (ha 'content))
;;   (pp (ha 'lookup 2 identity (const #f)))
;;   (pp (ha 'lookup 100 identity (const #f)))
;;   (pp (ha 'lookup 200 identity (const #f)))
;;   (ha 'insert! 1 'aa)
;;   (ha 'remove! 2)
;;   (ha 'remove! 200)
;;   (pp (ha 'content)))
