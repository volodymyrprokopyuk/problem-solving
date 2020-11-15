(define (=iota stop)
  "Returns a list of numbers from 0 till stop inclusive"
  (let ([kl (call/cc (lambda (k) (cons k (list stop))))])
    (let* ([k (car kl)] [l (cdr kl)] [i (car l)])
      (cond
        [(zero? i) l]
        [else (k (cons k (cons (- i 1) l)))]))))

;; #?=(=iota 10)

(define (=iota2 stop)
  "Returns a list of numbers from 0 till stop inclusive"
  (receive (k l i) (call/cc (lambda (k) (values k (list stop) stop)))
    (cond
      [(zero? i) l]
      [else (k k (cons (- i 1) l) (- i 1))])))

;; #?=(=iota2 10)
