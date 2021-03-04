(define-module dstructure)

(select-module dstructure)

(define-class <avector> ()
  ((capacity :init-keyword :capacity :init-value 10 :getter capacity)
   (length :init-value 0 :accessor length)
   (position :init-value 0 :accessor position)
   (array :accessor array)))

(define-method initialize ([v <avector>] _)
  (next-method)
  (set! (array v) (make-vector (capacity v))))

(define-method write-object ([v <avector>] p)
  (format p "#<~a ~a of ~a at ~a as ~a>"
          (class-name (current-class-of v))
          (length v) (capacity v) (position v) (array v)))

(let ([v (make <avector> :capacity 5)])
  (describe v)
  (print v))
