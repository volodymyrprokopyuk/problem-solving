(define-module hr
  (export <employee> <manager>))

(select-module hr)

(define-class <employee> ()
  ([name :init-keyword :name :init-value "nobody"]))

(define-class <manager> (<employee>) ())
