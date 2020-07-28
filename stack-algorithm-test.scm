(use-modules
 (ice-9 match) ;; match-lambda
 (srfi srfi-64) ;; Testing library
 (stack-algorithm))

(test-begin "stack-algorithm-test")

(for-each
 (match-lambda
   [(args expected)
    (test-eqv "evalutate-postfix: should work correctly"
      expected (evalutate-postfix args))])
 '(["10 20 +" 30]
   ["10 20 -" -10]
   ["10 20 *" 200]
   ["10 20 /" 1/2]
   ["-10 -20 +" -30]
   ["10 20 30 * +" 610]
   ["10 20 + 30 40 - *" -300]))

(for-each
 (match-lambda
   [(args expected)
    (test-eq "validate-parentheses should work correctly"
      expected (validate-parentheses args))])
 '(["(Vlad {and [Lana]})[ok]{done}" #t]
   ["{ (Vlad {and [Lana]})[ok]{done}" #f]
   ["(Vlad {and [Lana]})[o)k]{done}" #f]
   ["(Vlad {and [Lana]})[o)k]{done} ]" #f]))

(test-end "stack-algorithm-test")
