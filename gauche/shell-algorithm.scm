(define-module shell-algorithm
  (export cat-main grep-main))

(select-module shell-algorithm)

(define (cat-main args)
  "Prints the input files from the comman line args to the output port"
  (cond
    [(null? (cdr args))
     (copy-port (current-input-port) (current-output-port))]
    [else
     (for-each
      (lambda (f) (call-with-input-file f
               (lambda (ip) (copy-port ip (current-output-port)))))
      (cdr args))])
  0)

;; (define main cat-main)

(define (grep-usage program-name)
  (format (current-error-port) "Usage: ~a regexp file ...\n" program-name)
  (exit 2))

(define (grep rx ip)
  "Prints matched by the regexp rs lines from the input port ip to the output port"
  (with-input-from-port ip
    (lambda ()
      (port-for-each
       (lambda (line)
         (when [rxmatch rx line]
           (format #t "~a:~a: ~a\n" (port-name ip) (port-current-line ip) line)))
       read-line))))

(define (grep-main args)
  (cond
    [(null? (cddr args)) (grep-usage (car args))]
    [else
     (let ([rx (string->regexp (cadr args))])
       (for-each
        (lambda (f) (call-with-input-file f (lambda (ip) (grep rx ip))))
        (cddr args)))])
  0)

;; (define main grep-main)
