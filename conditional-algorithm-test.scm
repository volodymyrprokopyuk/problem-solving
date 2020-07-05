(use-modules
 (srfi srfi-64)
 (ice-9 match)
 (conditional-algorithm))

(test-begin "conditional-algorithm-test")

(for-each
 (match-lambda
   ([args expected]
    (test-eqv "at-least: should work correctly"
      expected (apply at-least args))))
 `(((,odd? ()) #f)
   ((,odd? (2 4 6)) #f)
   ((,odd? (2 1 4 6)) 1)
   ((,odd? (2 1 4 3 6)) 1)))

(for-each
 (match-lambda
   ([args expected]
    (test-eqv "at-most: should work correctly"
      expected (apply at-most args))))
 `(((,odd? ()) #f)
   ((,odd? (2 4 6)) #f)
   ((,odd? (2 1 4 6)) 1)
   ((,odd? (2 1 4 3 6)) #f)))

(test-end "conditional-algorithm-test")
