(define-syntax =when
  (syntax-rules ()
    [(_ p e ...) (if p (begin e ...))]))

;; #?=(macroexpand '(=when #t (display 'a) (display 'b)))

;; (trace-macro '=when)
;; (=when #t (display 'a) (display 'b))

(define-syntax =while
  (syntax-rules ()
    [(_ c e ...) (do () ((not c)) e ...)]))

;; (let ([i 0]) (=while (< i 5) (print i) (set! i (+ i 1))))
