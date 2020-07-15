(use-modules
 (ice-9 match) ;; match-lambda
 (srfi srfi-64) ;; Testing library
 (conditional-algorithm))

(test-begin "conditional-algorithm-test")

(for-each
 (match-lambda
   ([args expected]
    (test-eqv "at-least: should work correctly"
      expected (apply at-least args))))
 `([(,odd? ()) #f]
   [(,odd? (2 4 6)) #f]
   [(,odd? (2 1 4 6)) 1]
   [(,odd? (2 1 4 3 6)) 1]))

(for-each
 (match-lambda
   ([args expected]
    (test-eqv "at-most: should work correctly"
      expected (apply at-most args))))
 `([(,odd? ()) #f]
   [(,odd? (2 4 6)) #f]
   [(,odd? (2 1 4 6)) 1]
   [(,odd? (2 1 4 3 6)) #f]))

(for-each
 (match-lambda
   ([args expected]
    (test-equal "make-domain: should work correctly"
      expected (apply make-domain args))))
 '([((0 1) 0) (())]
   [((0 1) 1) ((0) (1))]
   [((0 1) 2) ((0 0) (0 1) (1 0) (1 1))]
   [((0 1) 3) ((0 0 0) (0 0 1) (0 1 0) (0 1 1) (1 0 0) (1 0 1) (1 1 0) (1 1 1))]))

(for-each
 (match-lambda
   ([args expected]
    (test-equal "make-truth-table: should work correctly"
      expected (apply make-truth-table args))))
 `([(,not 1) (((#f) . #t)
              ((#t) . #f))]
   [(,and* 2) (((#f #f) . #f)
               ((#f #t) . #f)
               ((#t #f) . #f)
               ((#t #t) . #t))]
   [(,or* 2) (((#f #f) . #f)
              ((#f #t) . #t)
              ((#t #f) . #t)
              ((#t #t) . #t))]
   [(,xor 2) (((#f #f) . #f)
              ((#f #t) . #t)
              ((#t #f) . #t)
              ((#t #t) . #f))]
   [(,equiv 2) (((#f #f) . #t)
                ((#f #t) . #f)
                ((#t #f) . #f)
                ((#t #t) . #t))]))

(for-each
 (match-lambda
   ([args expected]
    (test-equal "solve-quadratic-equation: should work correctly"
      expected (apply solve-quadratic-equation args))))
 '([(1) (0 . 0)]
   [(1 0 -4) (2 . -2)]
   [(1 -1 -2) (2 . -1)]
   [(1 0 4) (0.0+2.0i . -0.0-2.0i)]))

(for-each
 (match-lambda
   ([args expected]
    (test-eq "palindrome?: should work correctly"
      expected (palindrome? args))))
 '(["Madam" #t]
   ["Racecar" #t]
   ["Never odd or even" #t]
   ["Scheme" #f]))

(test-equal "number-palindromes: should work correctly"
  '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "11" "22" "33" "44" "55" "66" "77" "88" "99")
  (number-palindromes 100))

(test-end "conditional-algorithm-test")
