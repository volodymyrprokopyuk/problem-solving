(define-module
  (recursive-algorithm)
  #:export
  (factorial fibonacci meven? modd? hanoi power-set permute umap mmap power
             combinations-number minimax string-symmetric?))

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

(define (power x n)
  "Returns power x to n"
  (if (positive? n)
      (let power* ([i n] [r 1])
        (if [zero? i] r (power* (1- i) (* x r))))
      (let power* ([i n] [r 1])
        (if [zero? i] r (power* (1+ i) (* (/ 1 x) r))))))

(define (combinations-number n k)
  "Returns the number of combination of k from n without repetition"
  (if [or (zero? k) (= k n)] 1
      (+ (combinations-number (1- n) k) (combinations-number (1- n) (1- k)))))

(define (combinations-number2 n k)
  "Returns the number of combination of k from n without repetition"
  (/ (factorial n) (* (factorial k) (factorial (- n k)))))

(define (minimax l compare)
  "Returns minimum or maximum element of the list"
  (let minimax* ([l l] [r #f])
    (cond
      [(null? l) r]
      [(not r) (minimax* (cdr l) (car l))]
      [(compare (car l) r) (minimax* (cdr l) (car l))]
      [else (minimax* (cdr l) r)])))

(define (string-symmetric? s)
  "Returns #t on symmetric string"
  (let symm* ([ss s] [st '()] [ac 'push])
    (cond
      ;; At the end of a symmetric string stack should be empty
      [(string-null? ss) (null? st)]
      ;; Push the first character into the stack
      [(null? st)
       (symm* (string-drop ss 1) (cons (string-ref ss 0) st)
              (if (= (- (/ (string-length s) 2) (length st)) 1) 'pop 'push))]
      ;; Push the first half of the string into the stack
      [(and (eq? ac 'push) (< (length st) (/ (string-length s) 2)))
       (symm* (string-drop ss 1) (cons (string-ref ss 0) st)
              (if (= (- (/ (string-length s) 2) (length st)) 1) 'pop 'push))]
      ;; Check the second half of the string with the stack content
      [(and (eq? ac 'pop) (eqv? (string-ref ss 0) (car st)))
       (symm* (string-drop ss 1) (cdr st) 'pop)]
      ;; Short-circuit on the first unmatch
      [else #f])))
