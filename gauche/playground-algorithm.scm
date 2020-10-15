(define pp print)

;; #?=(+ 1 2)
;; #?,(+ 1 2)

;; #?=(values 1 2 3)
;; #?,((lambda (a) (values a 1 2 3)) 0)

(pp (sys-uid->user-name (sys-getuid)))
