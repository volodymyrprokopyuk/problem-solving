(define-module hrm
  (export <employee> <manager>))

(select-module hrm)

(define-class <employee> ()
  ([name :init-keyword :name :init-value "nobody" :accessor name]))

(define-class <manager> (<employee>) ())
