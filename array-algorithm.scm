(define-module
  (array-algorithm)
  #:export (vector-random-integer vector-min-max-swap!))

(use-modules
 (srfi srfi-27) ;; Random source
 (srfi srfi-133) ;; Vector library
 ((ice-9 pretty-print)
  #:select ((pretty-print . pp))))

(random-source-randomize! default-random-source)

(define (vector-random-integer vsize irange)
  "Generatges a random vector of integers within the range of the size"
  (vector-unfold (lambda (_) (random-integer irange)) vsize))

(define (vector-min-max-swap! v)
  "Swaps in-place minimal element with maximal element"
  (let* ([mi (vector-fold min (vector-ref v 0) v)]
         [ma (vector-fold max (vector-ref v 0) v)]
         [mi-index (vector-index (lambda (e) (= e mi)) v)]
         [ma-index (vector-index (lambda (e) (= e ma)) v)])
    (vector-swap! v mi-index ma-index)))

;; (let ([v (vector-random-integer 10 100)])
;;   (pp v) (vector-min-max-swap! v) (pp v))
