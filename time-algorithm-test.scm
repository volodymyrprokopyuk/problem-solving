(use-modules
 (ice-9 match) ;; match-lambda
 (srfi srfi-64) ;; Testing library
 (time-algorithm))

(test-begin "time-algorithm-test")

(for-each
 (match-lambda
   [(args expected)
    (test-eq "leap-year? should work correctly"
      expected (leap-year? args))])
 '([0 #t]
   [1 #f]
   [4 #t]
   [100 #f]
   [101 #f]
   [104 #t]
   [400 #t]
   [401 #f]
   [404 #t]
   [500 #f]
   [800 #t]
   [2000 #t]
   [2020 #t]))

(for-each
 (match-lambda
   [(args expected)
    (test-eqv "next-leap-year: whould work correctly"
      expected (next-leap-year args))])
 '(["1995-01-01" 1996]
   ["1996-01-01" 2000]
   ["2001-01-01" 2004]
   ["2096-01-01" 2104]))

(test-end "time-algorithm-test")
