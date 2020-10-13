(define-module
  (control-algorithm)
  #:export ())

(use-modules
 (ice-9 receive)
 (srfi srfi-34) ;; Exceptions
 (srfi srfi-35) ;; Conditions
 ((data-structure)
  #:select (make-queue qu-empty? qu-enqueue qu-dequeue qu-front qu-content))
 ((ice-9 pretty-print)
  #:select ((pretty-print . pp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Conditions and errors

(define-condition-type &base-error &condition
  base-error?
  (code error-code)
  (message error-message)
  (reason error-reason))

(define-condition-type &generic-error &base-error generic-error?)

(define-condition-type &specific-error &base-error specific-error?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exception heandlers and guards

#;(call/cc
 (lambda (k)
   (with-exception-handler
     (lambda (c) (format #t "ERROR: ~s\n" c) (k c))
     (lambda () ((let ([c
                   (condition
                    (&base-error (code 'E0001)
                                 (message "Base error")
                                 (reason "Base error reason")))])
              (raise c)))))))

#;(guard
 (c
  [(generic-error? c) (format #t "GENERIC ERROR: ~s\n" c)]
  [(specific-error? c) (format #t "SPECIFIC ERROR: ~s\n" c)]
  [else (format #t "BASE ERROR: ~s\n" c)])
 (let ([c
        (condition
         (&specific-error (code 'E0001)
                          (message "Base error")
                          (reason "Base error reason")))])
   (raise c)))

#;(let* ([&base-error2 (make-condition-type '&base-error2 &condition
                                          '(code message reason))]
       [c (make-condition &base-error2 'code 'E0001
                          'message "Base error"
                          'reason "Base error reason")])
  (guard
   (cc
    [else (format #t "ERROR 2: ~s" cc)])
   (raise c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (call/cc (lambda (k) (k value)))
;;   - call/cc captures the continuation k of the expression of call/cc application
;;   - each time k is applied to the value(s), the value(s) is passed to the
;;     contination of call/cc application
;;   - continuation k is an escape procedure from the (lambda (k)) context
;;   = escape procedure does not return to the point of its involation,
;;     but abandons its context and yeilds the result to the call/cc context
;; (call/cc allows for arbitrary (non-local) flow control manipulation

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Escape from infinite loop

(define (forever t)
  "Applies the thunk t forever"
  (t) (forever t))

(define (infinite-loop)
  (let ([c 0])
    (forever (lambda () (pp c) (set! c (1+ c))))))

;; (infinite-loop)

(define (escape-infinite-loop threshold)
  (call/cc
   (lambda (k)
     (let ([c 0])
       (forever
        (lambda ()
          (cond
            [(> c threshold) (k 'done)]
            [else (pp c) (set! c (1+ c))])))))))

;; (pp (escape-infinite-loop 10))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Escape from flat recursion

(define (product-flat l)
  (cond
    [(null? l) 1]
    [else (pp (car l)) (* (car l) (product-flat (cdr l)))]))

;; (pp (product-flat '(1 2 0 3 4 5)))

(define (product-flat-escape-on-zero l)
  (call/cc
   (lambda (k)
     (let product* ([l l])
       (cond
         [(null? l) 1]
         [(zero? (car l)) (k 0)]
         [else (pp (car l)) (* (car l) (product* (cdr l)))])))))

;; (pp (product-flat-escape-on-zero '(1 2 0 3 4 5)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Escape from deep recursion

(define (product-deep l)
  (cond
    [(null? l) 1]
    [(pair? (car l)) (* (product-deep (car l)) (product-deep (cdr l)))]
    [else (pp (car l)) (* (car l) (product-deep (cdr l)))]))

;; (pp (product-deep '(1 (2) 0 (3 (4)) 5)))

(define (product-deep-escape-on-zero l)
  (call/cc
   (lambda (k)
     (let product* ([l l])
       (cond
         [(null? l) 1]
         [(pair? (car l)) (* (product* (car l)) (product* (cdr l)))]
         [(zero? (car l)) (k 0)]
         [else (pp (car l)) (* (car l) (product* (cdr l)))])))))

;; (pp (product-deep-escape-on-zero '(1 (2) 0 (3 (4)) 5)))
;; (pp (product-deep-escape-on-zero '(1 (2) (3 (4 0 1)) 5)))

(define (search t l)
  (call/cc
   (lambda (k)
     ;; (for-each (lambda (e) (when [equal? e t] (k e))) l) #f
     (do ([l l (cdr l)]) ([null? l] #f) (when [equal? (car l) t] (k (car l)))))))

;; (pp (search 'c '(a b c d)))
;; (pp (search 'C '(a b c d)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; call/cc examples

;; Create (call/cc), parametrize (k 3), use (+ 1 []), and discard continuation
;; (pp (+ 1 (call/cc (lambda (k) (+ 2 (k 3))))))

;; Create (call/cc), store [f], parametrize (lambda (_)), and use (f) continuation
;; (pp (let ([f (call/cc (lambda (k) k))])
;;       (f (lambda (_) 'ok))))

;; Create (call/cc), parametrize (lambda), parametrize 2 ('ok), use ((call/cc))
;; (pp (((call/cc (lambda (k) k)) (lambda (x) x)) 'ok))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loop with one continuation

(define (loop-until t)
  (let ([ki (call/cc (lambda (k) (cons k 0)))])
    (let ([k (car ki)] [i (cdr ki)])
      (when [< i t] (pp i) (k (cons k (1+ i)))))))

;; (loop-until 10)

(define (loop-until2 t)
  (receive (k i) (call/cc (lambda (k) (values k 0)))
    (when [< i t] (pp i) (k k (1+ i)))))

;; (loop-until2 10)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interleaved multitasking

(define (compute-a k)
  (let compute* ([i 3])
    (format #t "compute-a: ~s~s\n" 'A i)
    (set! k (call/cc k))
    (format #t "compute-a: ~s~s\n" 'B i)
    (set! k (call/cc k))
    (unless [zero? i] (compute* (1- i)))))

(define (compute-b k)
  (let compute* ()
    (for-each
     (lambda (a)
       (format #t "compute-b ~s\n" a)
       (set! k (call/cc k)))
     '(1 2 3))
    (compute*)))

;; (compute-a compute-b)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Non-preemptive multitasking

#;(let ([task-queue (make-queue)] [quitk #f] [i 0])
  (define (add-task t)
    (set! task-queue (qu-enqueue task-queue t)))
  (define (start)
    (call/cc (lambda (k) (set! quitk k) (execute))))
  (define (execute)
    (receive (t qu) (qu-dequeue task-queue)
      (set! task-queue qu) (t)))
  (define (pause)
    (call/cc (lambda (k) (add-task (lambda () (k #f))) (execute))))
  (define (quit)
    (if [qu-empty? task-queue] (quitk #f) (execute)))
  (add-task
   (lambda ()
     (let f* ()
       (set! i (1+ i))
       (cond
         [(> i 10) (quit)]
         [else (pause) (display 'o) (f*)]))))
  (add-task
   (lambda ()
     (let f* ()
       (set! i (1+ i))
       (cond
         [(> i 11) (quit)]
         [else (pause) (display 'k) (f*)]))))
  (add-task
   (lambda ()
     (let f* ()
       (set! i (1+ i))
       (cond
         [(> i 12) (quit)]
         [else (pause) (newline) (f*)]))))
  (start))

#;(let ([i 0])
  (let ([a ((lambda (kk) (set! i (1+ i)) (display "@") kk) (call/cc (lambda (k) k)))]
        [b ((lambda (kk) (set! i (1+ i)) (display "*") kk) (call/cc (lambda (k) k)))])
    (when [< i 100] (a b))))
