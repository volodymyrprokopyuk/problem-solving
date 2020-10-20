(use-modules
 (ice-9 match) ;; match-lambda
 (srfi srfi-42) ;; Comprehensions
 (srfi srfi-64) ;; Testing library
 (iterative-algorithm))

(test-begin "iterative-algorithm-test")

(for-each
 (match-lambda
   [(args expected)
    (test-equal "range-between: should work correctly"
      expected (apply range-between args))])
 '([(0 0) (0)]
   [(0 1) (0 1)]
   [(0 2) (0 1 2)]))

(test-error "range-between: should raise error"
  #t (range-between 2 1))

(for-each
 (match-lambda
   [(args expected)
    (test-equal "integer-factorization: should work correctly"
      expected (integer-factorization args))])
 '([2 (2)]
   [3 (3)]
   [4 (2 2)]
   [5 (5)]
   [6 (2 3)]
   [7 (7)]
   [8 (2 2 2)]
   [9 (3 3)]
   [10 (2 5)]
   [11 (11)]
   [12 (2 2 3)]
   [13 (13)]
   [14 (2 7)]
   [15 (3 5)]
   [16 (2 2 2 2)]
   [1047552 (2 2 2 2 2 2 2 2 2 2 3 11 31)]
   [111111111111 (3 7 11 13 37 101 9901)]))

(test-error "integer-factorization: should raise error"
  (integer-factorization 1))

;; (test-equal "perfect-numbers: should work correctly"
;;   '(6 28 496 8128) (perfect-numbers 10000))

(test-equal "prime-numbers: should work correctly"
  '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97)
  (prime-numbers 100))

(for-each
 (match-lambda
   [(args expected)
    (test-eqv "greatest-common-divisor: should work correctly"
      expected (apply greatest-common-divisor args))])
 '([(1) 1]
   [(1 1) 1]
   [(1 2) 1]
   [(2 4) 2]
   [(3 12) 3]
   [(6 15) 3]
   [(36 24) 12]
   [(6 9 12 15) 3]))

(for-each
 (match-lambda
   [(args expected)
    (test-eqv "lowest-common-multiple: should work correctly"
      expected (apply lowest-common-multiple args))])
 '([(1) 1]
   [(1 1) 1]
   [(1 2) 2]
   [(4 2) 4]
   [(5 2) 10]
   [(18 12) 36]
   [(5 12 15) 60]))

(for-each
 (match-lambda
   [(args expected)
    (test-equal "happy-tickets: should work correcly"
      expected (apply happy-tickets args))])
 '([(1 1) ("0" "1" "2" "3" "4" "5" "6" "7" "8" "9")]
   [(2 1) ("00" "11" "22" "33" "44" "55" "66" "77" "88" "99")]))

(test-equal "armstrong-numbers: should work correctly"
  '(0 1 2 3 4 5 6 7 8 9 153 370 371 407) (armstrong-numbers 1000))

(for-each
 (match-lambda
   [(args expected)
    (test-equal "dec->str: should work correctly"
      expected (apply dec->str args))])
 '([(33 2) "100001"]
   [(33 5) "113"]
   [(33 8) "41"]
   [(33) "33"]
   [(33 16) "21"]))

(for-each
 (match-lambda
   [(args expected)
    (test-equal "str->dec: should work correctly"
      expected (apply str->dec args))])
 '([("100001" 2) 33]
   [("113" 5) 33]
   [("41" 8) 33]
   [("33") 33]
   [("21" 16) 33]))

(for-each
 (match-lambda
   [(args expected)
    (test-equal "group-by: should work correctly"
      expected (apply group-by args))])
 `([(1 ,(list-ec (:range i 1 10) i))
    ((1) (2) (3) (4) (5) (6) (7) (8) (9))]
   [(2 ,(list-ec (:range i 1 10) i))
    ((1 2) (3 4) (5 6) (7 8) (9))]
   [(3 ,(list-ec (:range i 1 10) i))
    ((1 2 3) (4 5 6) (7 8 9))]
   [(4 ,(list-ec (:range i 1 10) i))
    ((1 2 3 4) (5 6 7 8) (9))]))

(test-end "iterative-algorithm-test")
