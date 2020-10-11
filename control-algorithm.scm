(define-module
  (control-algorithm)
  #:export ())

(use-modules
 (srfi srfi-34) ;; Exceptions
 (srfi srfi-35) ;; Conditions
 ((ice-9 pretty-print)
  #:select ((pretty-print . pp))))

(define-condition-type &base-error &condition
  base-error?
  (code error-code)
  (message error-message)
  (reason error-reason))

(define-condition-type &generic-error &base-error generic-error?)

(define-condition-type &specific-error &base-error specific-error?)

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
