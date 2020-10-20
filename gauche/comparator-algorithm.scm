(define-module comparator-algorithm)

(define dc default-comparator)

(define-class <point> ()
  ([x :init-keyword :x]
   [y :init-keyword :y]))

(define-method object-equal? ([a <point>] [b <point>])
  (and (equal? (slot-ref a 'x) (slot-ref b 'x))
       (equal? (slot-ref a 'y) (slot-ref b 'y))))

;; #?=(let ([a (make <point> :x 1 :y (list 1 2))]
;;          [b (make <point> :x 1 :y (list 1 2))])
;;      (values (equal? a b) (=? dc a b)))

(define-method object-compare ([a <point>] [b <point>])
  (case (compare (slot-ref a 'x) (slot-ref b 'x))
    [(-1 1) => identity]
    [else (compare (slot-ref a 'y) (slot-ref b 'y))]))

;; #?=(let ([a (make <point> :x 1 :y (list 1 2))]
;;          [b (make <point> :x 1 :y (list 1 2))])
;;      (values (compare a b) (comparator-compare dc a b) (<=? dc a b) (>? dc a b)))

(define-method object-hash ([a <point>] rec-hash)
  (combine-hash-value (rec-hash (slot-ref a 'x)) (rec-hash (slot-ref a 'y))))

;; #?=(let ([a (make <point> :x 1 :y (list 1 2))]
;;          [b (make <point> :x 1 :y (list 1 2 3))]
;;          [salt 123456789])
;;      (values (default-hash a) (portable-hash a salt) (comparator-hash dc a)
;;              (default-hash b) (portable-hash b salt) (comparator-hash dc b)))
