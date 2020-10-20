(define-module
  (string-algorithm)
  #:export (anagram?))

(use-modules
 (srfi srfi-1) ;; List library
 (srfi srfi-115) ;; IrRegEx
 (srfi srfi-69) ;; Hash table
 ((ice-9 pretty-print)
  #:select ((pretty-print . pp))))

;; (pp (let ([s "<em>Vlad</em> and <STRONG>Lana</STRONG>"]
;;           [tag->tag
;;            (alist->hash-table
;;             '(("<" . "[") (">" . "]")))]
;;           [r (irregex "(<)(/)?([^>]+)(>)")])
;;       (irregex-replace/all
;;        r s
;;        (lambda (m)
;;          (let ([op (irregex-substring m 1)]
;;                [sl (irregex-substring m 2)]
;;                [nm (irregex-substring m 3)]
;;                [cl (irregex-substring m 4)])
;;            (format
;;             #f "~a~a~a~a"
;;             (hash-table-ref tag->tag op)
;;             (or sl "") (string-downcase nm)
;;             (hash-table-ref tag->tag cl)))))))

(define (anagram? s1 s2)
  "Returns true if strings s1 and s2 contain exactly the same set of chars"
  " possibly in different order"
  (let ([l1 (sort (string->list (string-downcase s1)) char<?)]
        [l2 (sort (string->list (string-downcase s2)) char<?)])
    (list= eq? l1 l2)))

;; (pp (let ([s "Vlad and Lana"]
;;           [r (irregex "(\\w)(\\w)")])
;;       (irregex-replace/all r s 2 1)))

(pp (let* ([s "Vlad and Lana"]
           [r (irregex "\\A(\\w+)( .+ )(\\w+)\\Z")])
      (irregex-replace r s 3 2 1)))
