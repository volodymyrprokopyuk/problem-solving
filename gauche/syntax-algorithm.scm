(define-syntax =when
  (syntax-rules ()
    [(_ p . e) (if p (begin . e))]))

#?=(macroexpand '(=when #t (display 'a) (display 'b)))

(trace-macro '=when)
(=when #t (display 'a) (display 'b))
