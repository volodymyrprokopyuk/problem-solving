(define-module
  (file-algorithm)
  #:export ())

(use-modules
 (ice-9 textual-ports)
 (srfi srfi-9) ;; Records
 (srfi srfi-115) ;; IrRegEx
 ((ice-9 pretty-print)
  #:select ((pretty-print . pp))))

(define-record-type <city>
  (make-city  name population)
  city?
  (name city-name)
  (population city-population))

(define (city->string c)
  (format #f "#<<city> name: ~s population: ~s>" (city-name c) (city-population c)))

(define (string->city s)
  (let* ([r (irregex
             "#<<city> name: \"(?<name>[^\"]+)\" population: (?<population>\\d+)>")]
         [m (irregex-match r s)])
    (when (not m) (error "string->city: invalid city:" s))
    (let ([name (irregex-substring m 'name)]
          [population (irregex-substring m 'population)])
      (make-city name population))))

;; (let ([c (make-city "Madrid" 6)])
;;   (pp (string->city (city->string c))))

(define (write-cities f)
  (let* ([cs (list (make-city "Madrid" 6)
                   (make-city "London" 8)
                   (make-city "Paris" 7))]
         [s (string-join (map city->string cs) "\n")])
    (call-with-output-file f
      (lambda (port)
        (put-string port s)))))

;; (write-cities "cities.txt")

(define (read-cities f)
  (let* ([s (call-with-input-file f
              (lambda (port)
                (get-string-all port)))])
    (map string->city (irregex-split "\n" s))))

;; (pp (read-cities "cities.txt"))
