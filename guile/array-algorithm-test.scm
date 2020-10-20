(use-modules
 (ice-9 match) ;; match-lambda
 (srfi srfi-64) ;; Testing library
 (array-algorithm))

(test-begin "array-algorithm-test")

(for-each
 (match-lambda
   [(args expected)
    (test-equal "split-in-chunks: should work correctly"
      expected (apply split-in-chunks args))])
 '([(() 1) ()]
   [((1) 1) ((1))]
   [((1 2) 1) ((1) (2))]
   [((1) 2) ((1))]
   [((1 2) 2) ((1 2))]
   [((1 2 3) 2) ((1 2) (3))]
   [((1 2 3 4 5) 3) ((1 2 3) (4 5))]
   [((1 2 3 4 5 6) 3) ((1 2 3) (4 5 6))]))

(for-each
 (match-lambda
   [(args expected)
    (test-equal "split-in-overlaps: should work correctly"
      expected (apply split-in-overlaps args))])
 '([(() 1) (())]
   [((1) 1) ((1))]
   [((1 2) 1) ((1) (2))]
   [((1 2) 2) ((1 2))]
   [((1 2 3) 2) ((1 2) (2 3))]
   [((1 2 3) 3) ((1 2 3))]
   [((1 2 3 4) 3) ((1 2 3) (2 3 4))]
   [((1 2 3 4 5) 3) ((1 2 3) (2 3 4) (3 4 5))]))

(test-end "array-algorithm-test")
