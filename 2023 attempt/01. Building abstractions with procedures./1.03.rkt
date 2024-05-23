#lang sicp
(define (square x) (* x x))
(define (sum-of-squares x y) (+ (square x) (square y)))
(define (solution a b c)
  (cond ((and (< a b) (< a c)) (sum-of-squares b c))
        ((and (> a b) (< b c)) (sum-of-squares a c))
        (else (sum-of-squares a b))))


(solution 1 2 3) ; 13
(solution 4 2 3) ; 25
(solution 0 0 0) ; 0
(solution 1 0 1) ; 2
(solution 2 3 2) ; 13
