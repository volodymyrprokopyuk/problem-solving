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

(call/cc
 (lambda (k)
   (with-exception-handler
     (lambda (c) (format #t "ERROR: ~s\n" c) (k c))
     (lambda () ((let ([c
                   (condition
                    (&base-error (code 'E0001)
                                 (message "Base error")
                                 (reason "Base error reason")))])
              (raise c)))))))

(guard
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

(let* ([&base-error2 (make-condition-type '&base-error2 &condition
                                          '(code message reason))]
       [c (make-condition &base-error2 'code 'E0001
                          'message "Base error"
                          'reason "Base error reason")])
  (guard
   (cc
    [else (format #t "ERROR 2: ~s" cc)])
   (raise c)))
