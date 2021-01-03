(define-module pattern-matcher)

(select-module pattern-matcher)

(define (*match? p l)
  "Returns #t if the pattern p mathces zero or more times the list l"
  (let match* ([l l])
    (cond
      [(null? l) #f]
      [(pmatch? (cdr p) l) #t]
      [(not (eq? (car p) (car l))) #f]
      [else (match* (cdr l))])))

(define (&match? p l)
  "Returns #t if the pattern p mathces one or more times the list l"
  (let match* ([l l])
    (cond
      [(null? l) #f]
      [(not (eq? (car p) (car l))) #f]
      [(pmatch? (cdr p) (cdr l)) #t]
      [else (match* (cdr l))])))

(define (pmatch? p l)
  "Returns #t if the pattern p matches the list l. \
   Pattern rules: ! exactly once, ? zero or one, * zero or more, & one or more"
  (cond
    [(null? p) (null? l)]
    ;; backtracking (explicit or): tentative decision > recursive call > final decision
    ;;   - if recursive call matches, then tentative decision was correct
    ;;   - otherwise alternative decision > recursive call > final decision
    [(eq? (car p) '?) (or (pmatch? (cdr p) l) (pmatch? (cddr p) l))]
    ;; backtracking (recursive call)
    [(eq? (car p) '*) (*match? (cdr p) l)]
    [(eq? (car p) '&) (&match? (cdr p) l)]
    [(null? l) #f]
    [(eq? (car p) '!) (pmatch? (cdr p) l)]
    [(eq? (car p) (car l)) (pmatch? (cdr p) (cdr l))]
    [else #f]))

(define (main args)
  "Starts program execution"
  #?=(pmatch? '(a ? b ? c ! d * e f ? g & h ! i) '(a b c d e e f g h h i))
  0)
