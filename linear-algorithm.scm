(define-module
  (linear-algorithm)
  #:export (convert-size))

(define* (convert-size bytes unit #:key (precision 2))
  "Converts size in bytes into approximate size in unit (kb, mb, gb)"
  (let* ([p
          (case unit
            [(kb) 10]
            [(mb) 20]
            [(gb) 30]
            [else (error "convert-size: invalid unit:" unit)])]
         [s (/ bytes (expt 2 p))]
         [k (expt 10 precision)]
         [size (/ (round (* (exact->inexact s) k)) k)])
    (cons size unit)))
