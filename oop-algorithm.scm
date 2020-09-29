(define-module
  (relational-algorithm))

(use-modules
 (set-algorithm)
 ((ice-9 pretty-print)
  #:select ((pretty-print . pp))))

(define (delegate o m . args)
  "Delegate the method m with the arguments args to the object o"
  (apply o m args))

(define (make-box x)
  "Returns a box with the initial value x inside"
  (let ([v x])
    (lambda (m . args)
      (case m
        [(type) "box"]
        [(show) v]
        [(update!) (set! v (car args))]
        [(reset!) (set! v x)]
        [else (error "box: not supported method:" m)]))))

;; (let ([box (make-box 1)])
;;   (pp (box 'type))
;;   (pp (box 'show))
;;   (box 'update! 2)
;;   (pp (box 'show))
;;   (box 'reset!)
;;   (pp (box 'show)))

(define (make-counter x s)
  "Returns a counter with the initial value x and the unary step procedure s"
  (let ([c (make-box x)])
    (lambda (m . args)
      (case m
        [(type) "counter"]
        [(step!) (c 'update! (s (c 'show)))]
        ;; Delegate the show and reset! methods to the box
        [(show reset!) (delegate c m args)]
        [else (error "counter: not supported method:" m)]))))

;; (let ([counter (make-counter 0 1+)])
;;   (pp (counter 'type))
;;   (pp (counter 'show))
;;   (counter 'step!)
;;   (counter 'step!)
;;   (pp (counter 'show))
;;   (counter 'reset!)
;;   (pp (counter 'show)))
