(use-modules
 (ice-9 match) ;; match-lambda
 (srfi srfi-64) ;; Testing library
 (string-algorithm))

(test-begin "string-algorithm-test")

(for-each
 (match-lambda
   [(args expected)
    (test-eq "anagram?: should work correctly"
      expected (apply anagram? args))])
 '([("Race" "Care") #t]
   [("PART" "trap") #t]
   [("earth" "heart") #t]
   [("knee" "KEEN") #t]
   [("Lan" "Lana") #f]
   [("Vla" "Vlad") #f]))

(test-end "string-algorithm-test")
