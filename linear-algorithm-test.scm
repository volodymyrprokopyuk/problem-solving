(use-modules
 (ice-9 match) ;; match-lambda
 (srfi srfi-64) ;; test-begin
 (linear-algorithm))

(test-begin "linear-algorithm-test")

(for-each
 (match-lambda
   ([args expected]
    (test-equal "convert-size: should work correctly"
      expected (apply convert-size args))))
 '([(0 kb) (0.0 . kb)]
   [(1010 kb) (0.99 . kb)]
   [(1024 kb) (1.0 . kb)]
   [(1030 kb) (1.01 . kb)]
   [(1025 kb #:precision 3) (1.001 . kb)]
   [(1030 kb #:precision 1) (1.0 . kb)]
   [(1234567 mb) (1.18 . mb)]
   [(9876543210 gb) (9.2 . gb)]))

(for-each
 (lambda (args)
   (test-error "convert-size: should raise error"
     #t (apply convert-size args)))
 '([1 tb] [s kb]))

(test-end "linear-algorithm-test")
