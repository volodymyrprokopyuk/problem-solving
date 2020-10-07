(define-module
  (macro-algorithm))

(use-modules
 ((ice-9 pretty-print)
  #:select ((pretty-print . pp))))

(define-syntax =when
  (syntax-rules ()
    [(_ c e1 e2 ...) (if c (begin e1 e2 ...))]))

;; (=when #t (pp 'when) (pp 'true))

#;(letrec-syntax
    ([=or
      (syntax-rules ()
        [(_) #f]
        [(_ e) e]
        [(_ e1 e2 ...)
         (let ([b e1])
           (if b b (=or e2 ...)))])])
  (pp (=or #f 'b 'c)))

(define-syntax =unless
  (lambda (s)
    (syntax-case s ()
      [(_ c e1 e2 ...) #'(if (not c) (begin e1 e2 ...))])))

;; (=unless #f (pp 'unless) (pp 'false))
