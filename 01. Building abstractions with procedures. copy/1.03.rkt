#lang racket

; Define a procedure that takes three numbers as arguments and
; returns the sum of the squares of the two larger numbers.
(require rackunit)

(define (max-of-three a b c)
  (if (> a b)
      (if (> b c) (sum-of-squares a b)
          (sum-of-squares a c))
      (if (> a c) (sum-of-squares a b)
          (sum-of-squares b c))))


(define (sum-of-squares a b)
  (+ (* a a) (* b b)))

(check-equal? (max-of-three 1 2 3) 13)
(check-equal? (max-of-three 1 3 2) 13)
(check-equal? (max-of-three 2 1 3) 13)
(check-equal? (max-of-three 2 3 1) 13)