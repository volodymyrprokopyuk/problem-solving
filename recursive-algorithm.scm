(define-module
  (recursive-algorithm)
  #:export
  (factorial fibonacci meven? modd? hanoi power-set permute umap mmap))

(use-modules
 ((ice-9 pretty-print)
  #:select ((pretty-print . pp))))

(define (factorial n)
  "Calculates the factorial of the number n"
  (let factorial* ([n n] [r 1])
    ;; Direct recutsion
    (if (zero? n) r (factorial* (1- n) (* r n)))))

(define (fibonacci n)
  "Calculates the Fibonacci sequence value for the position n"
  (let fibonacci* ([n n] [a -1] [b 1])
    ;; Tail recursion
    (if (zero? n) (+ a b) (fibonacci* (1- n) b (+ a b)))))

(define (meven? n)
  "Returns true if the number n is even"
  ;; Indirect recursion
  (if (zero? n) #t (modd? (1- n))))

(define (modd? n)
  "Returns true if the number n is odd"
  ;; Indirect recursion
  (if (zero? n) #f (meven? (1- n))))

;; Tower of Hanoi (side effecting solution)
(define (pp-hanoi n source target spare)
  "Finds the solution for the Tower of Hanoi problem for number of disks n"
  (cond
    [(= n 1) (pp (cons source target))]
    [else
     ;; Move (n - 1) disks from source to spare
     (pp-hanoi (1- n) source spare target)
     ;; Move the biggest last disk from source to target
     (pp-hanoi 1 source target spare)
     ;; Mothe the previously moded (n - 1) disks from spare to target
     (pp-hanoi (1- n) spare target source)]))

;; (pp-hanoi 1 'source 'target 'spare) (newline)
;; (pp-hanoi 2 'source 'target 'spare) (newline)
;; (pp-hanoi 3 'source 'target 'spare)

;; Tower of Hanoi (functional solution)
(define (hanoi n source target spare)
  "Finds the solution for the Tower of Hanoi problem for number of disks n"
  (cond
    [(zero? n) '()]
    [else
     ;; Combine all moves
     (append
      ;; Move (n - 1) disks from source to spare
      (hanoi (1- n) source spare target)
      ;; Move the biggest last disk from source to target
      (list (cons source target))
      ;; Mothe the previously moded (n - 1) disks from spare to target
      (hanoi (1- n) spare target source))]))

(define (power-set l)
  "Builds a set of all subsets of the list l including the empty set"
  ;; Empty set is a subset of itself
  (if [null? l] '(())
      (let ([s (power-set (cdr l))])
        ;; Combine the power set of all but the first element
        ;; with itself prepended with the first element
        (append s (map (lambda (ss) (cons (car l) ss)) s)))))

(define (permute l)
  "Builds all permutations of the list l"
  (if [null? l] '(())
      ;; Combine all permutations
      (apply append
             ;; For each element in the list
             (map (lambda (e)
                    ;; Prepend the element to a subset of all permutations
                    ;; without this element
                    (map (lambda (p) (cons e p)) (permute (delete e l))))
                  l))))

(define (umap f l)
  "Maps the function f of one argument over the list l"
  (if [null? l] '()
      (cons (f (car l)) (umap f (cdr l)))))

(define (mmap f l . ll)
  "Maps the function f of multiple arguments over the list l"
  (if [null? l] '()
      ;; Construct list from results of applying function f
      ;; to first elements of all lists
      (cons (apply f (cons (car l) (umap car ll)))
            ;; Recursively apply function f to the corresponding elements of all lists
            (apply mmap (cons f (cons (cdr l) (umap cdr ll)))))))
