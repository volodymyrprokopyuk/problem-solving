(use-modules
 (ice-9 match) ;; match-lambda
 (srfi srfi-64) ;; Testing library
 (recursive-algorithm))

(test-begin "recursive-algorithm-test")

(test-equal "factorial: should work correctly"
  '(1 1 2 6 24 120 720) (map factorial (iota 7)))

(test-equal "fibonacci: should work correctly"
  '(0 1 1 2 3 5 8 13 21 34) (map fibonacci (iota 10)))

(test-equal "meven? modd?: should work correctly"
  '(#f . #t) (cons (meven? 7) (modd? 7)))

(for-each
 (match-lambda
   ([args expected]
    (test-equal "hanoi: should work correctly"
      expected (apply hanoi args))))
 '([(1 'source 'target 'spare)
    (('source . 'target))]
   [(2 'source 'target 'spare)
    (('source . 'spare)
     ('source . 'target)
     ('spare . 'target))]
   [(3 'source 'target 'spare)
    (('source . 'target)
     ('source . 'spare)
     ('target . 'spare)
     ('source . 'target)
     ('spare . 'source)
     ('spare . 'target)
     ('source . 'target))]))

(for-each
 (match-lambda
   ([args expected]
    (test-equal "power-set: should work correctly"
      expected (power-set args))))
 '([() (())]
   [(1) (() (1))]
   [(1 2) (() (2) (1) (1 2))]
   [(1 2 3) (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))]))

(for-each
 (match-lambda
   ([args expected]
    (test-equal "permute: should work correctly"
      expected (permute args))))
 '([() (())]
   [(1) ((1))]
   [(1 2) ((1 2) (2 1))]
   [(1 2 3) ((1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 1 2) (3 2 1))]))

(test-equal "umap: should work correctly"
  '(11 21 31 41 51) (umap 1+ (iota 5 10 10)))

(test-equal "mmap: should work correctly"
  '((a A) (b B) (c C)) (mmap cons '(a b c) '((A) (B) (C))))

(test-end "recursive-algorithm-test")
