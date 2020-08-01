(define-module
  (stack-algorithm)
  #:export
  (evalutate-postfix validate-parentheses))

(use-modules
 (ice-9 receive)
 (srfi srfi-11) ;; values
 (srfi srfi-1) ;; List library
 (srfi srfi-69) ;; Hash table
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
              [(string=? t "+") (calculate + st)]
              [(string=? t "-") (calculate - st)]
              [(string=? t "*") (calculate * st)]
              [(string=? t "/") (calculate / st)]
              [else (push (string->number t) st)]))]
         [ts (string-tokenize s cs)]
         [st (fold evaluate (make-stack) ts)])
    (receive (r st) (pop st)
      r)))

(define (validate-parentheses s)
  "Validates the correct order of possibly nested opening and closing parentheses"
  (let* ([op (char-set #\( #\[ #\{)]
         [cl (char-set #\) #\] #\})]
         [cs (char-set-union op cl)]
         [cl->op
          (alist->hash-table
           (map cons (char-set->list cl) (char-set->list op)))]
         [ts (string-fold-right cons '() (string-filter cs s))]
         [validate
          (lambda (ts)
            (let validate* ([ts ts] [st (make-stack)])
              (cond
                [(null? ts) (values ts st)]
                [(char-set-contains? op (car ts))
                 (validate* (cdr ts) (push (car ts) st))]
                [(char=? (hash-table-ref cl->op (car ts)) (peek st))
                 (validate* (cdr ts) (receive (_ st) (pop st) st))]
                [else (values ts st)])))])
    (receive (ts st) (validate ts)
      (and (null? ts) (stack-null? st)))))
