(define-module (linear-algorithm)
  #:use-module ((ice-9 pretty-print)
                #:select ((pretty-print . pp)))
  #:export (convert-size))

(define* (convert-size bytes unit #:key (precision 2))
  "Converts size in bytes into approximate size in unit (kb, mb, gb)"
  (let* ([p
          (cond
            [(eq? unit 'kb) 10]
            [(eq? unit 'mb) 20]
            [(eq? unit 'gb) 30]
            [else (error "convert-size: invalid unit:" unit)])]
         [s (/ bytes (expt 2 p))]
         [k (expt 10 precision)]
         [size (/ (round (* (exact->inexact s) k)) k)])
    (cons size unit)))

(pp (convert-size 0 'kb))
(pp (convert-size 1010 'kb))
(pp (convert-size 1024 'kb))
(pp (convert-size 1030 'kb))
(pp (convert-size 1025 'kb #:precision 3))
(pp (convert-size 1030 'kb #:precision 1))
(pp (convert-size 3198260 'mb))
;; (pp (convert-size 3198260 'tb))
