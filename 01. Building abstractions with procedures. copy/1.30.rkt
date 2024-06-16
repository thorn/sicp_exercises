#lang racket
#|
The sum procedure above generates a linear recursion. The procedure can be rewritten so that the sum is performed iteratively. Show how to do this by filling in the missing expressions in the following definition:

(define (sum term a next b)
  (define (iter a result)
    (if ⟨??⟩
        ⟨??⟩
        (iter ⟨??⟩ ⟨??⟩)))
  (iter ⟨??⟩ ⟨??⟩))
|#


(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))


(define (cube n) (* n n n))
(define (inc n) (+ n 1))
(define (sum-cubes b)
  (sum cube 1 inc b))
(sum-cubes 5) ; 1**3 + 2**3 + 3**3 + 4**3 + 5**3 = 225