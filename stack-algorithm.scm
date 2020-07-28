(define-module
  (stack-algorithm)
  #:export
  (evalutate-postfix))

(use-modules
 (ice-9 receive)
 (srfi srfi-1) ;; List library
 (srfi srfi-11) ;; Values
 (data-structure)
 ((ice-9 pretty-print)
  #:select ((pretty-print . pp))))

(define (evalutate-postfix s)
  "Evalutates postfix arithmetic expression"
  (let* ([cs (char-set-union char-set:digit (char-set #\+ #\- #\* #\/))]
         [calculate
          (lambda (op st)
            (let*-values ([(b st) (pop st)]
                          [(a st) (pop st)])
              (push (op a b) st)))]
         [evaluate
          (lambda (t st)
            (cond
              [(string= t "+") (calculate + st)]
              [(string= t "-") (calculate - st)]
              [(string= t "*") (calculate * st)]
              [(string= t "/") (calculate / st)]
              [else (push (string->number t) st)]))]
         [ts (string-tokenize s cs)]
         [st (fold evaluate (make-stack) ts)])
    (receive (r st) (pop st)
      r)))
