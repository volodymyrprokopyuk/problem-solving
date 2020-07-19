(define-module
  (array-algorithm)
  #:export
  (vector-random-integer vector-min-max-swap! split-in-chunks split-in-overlaps
                         list-miniax))

(use-modules
 (ice-9 match) ;; match-lambda
 (srfi srfi-1) ;; List library
 (srfi srfi-27) ;; Random source
 (srfi srfi-133) ;; Vector library
 ((ice-9 pretty-print)
  #:select ((pretty-print . pp))))

(random-source-randomize! default-random-source)

(define (vector-random-integer vlength irange)
  "Generatges a random vector of integers within the range of the length"
  (vector-unfold (lambda (_) (random-integer irange)) vlength))

(define (list-random-integer llength irange)
  "Generated a random list of integer within the range of the length"
  (unfold zero? (lambda (_) (random-integer irange)) (lambda (s) (1- s)) llength))

(define (vector-min-max-swap! v)
  "Swaps in-place minimal element with maximal element"
  (let* ([mi (vector-fold min (vector-ref v 0) v)]
         [ma (vector-fold max (vector-ref v 0) v)]
         [mi-index (vector-index (lambda (e) (= e mi)) v)]
         [ma-index (vector-index (lambda (e) (= e ma)) v)])
    (vector-swap! v mi-index ma-index)))

;; (let ([v (vector-random-integer 10 100)])
;;   (pp v) (vector-min-max-swap! v) (pp v))

(define (split-in-chunks l chlength)
  "Splits the list into non-overlapping chunks of the length"
  (let split* ([l l] [ch '()] [i 1] [r '()])
    (cond
      [(null? l)
       (if (null? ch) (reverse r) (reverse (cons (reverse ch) r)))]
      [(zero? (remainder i chlength))
       (split* (cdr l) '() (1+ i) (cons (reverse (cons (car l) ch)) r))]
      [else (split* (cdr l) (cons (car l) ch) (1+ i) r)])))

(define (split-in-overlaps l olength)
  "Splits the list into overlapping chunks of the length"
  (let split* ([l l] [ov '()] [i 0] [r '()])
    (cond
      [(null? l) (reverse (cons (reverse ov) r))]
      [(< i olength) (split* (cdr l) (cons (car l) ov) (1+ i) r)]
      [else
       (split* (cdr l) (cons (car l) (drop-right ov 1)) (1+ i)
               (cons (reverse ov) r))])))

(define (list-minimax l compare)
  "Returns the local minimums or local maximums based on compare function"
  (let ([overlaps (split-in-overlaps l 3)]
        [minimax?
         (match-lambda ([a b c] (and (compare b a) (compare b c))))])
    (filter minimax? overlaps)))

(let ([l (list-random-integer 10 100)])
  (pp l)
  (pp (list-minimax l <))
  (pp (list-minimax l >)))
