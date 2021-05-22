(use math.const)

(define (total-price p :optional (t 0.05)) (+ p (* p t)))

;; #?=(total-price 10)
;; #?=(total-price 10 0)

(define (cylinder-volume r h) (* h pi r r))

;; #?=(cylinder-volume 5 4)

(define (income-tax i :optional (t 0.2)) (cond [(< i 1e4) 0] [else (* i t)]))

;; #?=(income-tax 9000)
;; #?=(income-tax 30000)
