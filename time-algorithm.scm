(define-module
  (time-algorithm)
  #:export (duration-since leap-year? next-leap-year))

(use-modules
 (ice-9 receive)
 (srfi srfi-19) ;; Time date
 (srfi srfi-42) ;; Comprehensions
 ((ice-9 pretty-print)
  #:select ((pretty-print . pp))))

;; (pp (let* ([t1 (current-time)]
;;            [d (make-time time-duration 0 (* 60 60 24 365 2))]
;;            [t2 (add-duration t1 d)]
;;            [td (time-difference t2 t1)])
;;       ;; time difference in years
;;       (round (/ (time-second td) 60 60 24 365))))

;; (pp (let* ([t1 (current-time)]
;;            [d (make-time time-duration 0 (* 60 60 24 30 4))]
;;            [t2 (subtract-duration t1 d)]
;;            [td (time-difference t2 t1)]
;;            ;; time difference in months
;;            [md (round (/ (time-second td) 60 60 24 30))])
;;       (format #f "t2 ~a ~a month(s)" (if [positive? md] "later" "earlier") (abs md))))

;; (pp (let* ([d (current-date)])
;;       (date->string d "~Y-~m-~d ~H:~M:~S~z")))

;; (pp (date->string (current-date) "~A"))

(define (duration-since ds)
  "Returns the number of years, months, and days since the date string"
  (let* ([d (string->date ds "~Y-~m-~d")]
         [t (date->time-utc d)]
         [td (time-difference (current-time) t)]
         [days (quotient (time-second td) (* 60 60 24))]
         [year (quotient days 365)]
         [month (quotient (remainder days 365) 30)]
         [day (remainder (remainder days 365) 30)])
    (values year month day)))

;; (receive (year month day) (duration-since "2018-04-01")
;;   (pp (list year month day)))

(define (leap-year? y)
  "Returns #t if the year is a leap year"
  (or (zero? (remainder y 400))
      (and (zero? (remainder y 4))
           (not (zero? (remainder y 100))))))

(define (next-leap-year ds)
  "Returns the next leap year for a date"
  (let* ([d (string->date ds "~Y-~m-~d")]
         [year (date-year d)])
    (first-ec 0 (:range y (1+ year) (+ year 10)) (if (leap-year? y)) y)))
