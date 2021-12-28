#lang racket
(define (!= a b) (not (= a b)))

(define (random-integer-below n) (+ 1 (random (- n 1))))

(define (square x)
  (* x x))

(define (sqrmod x m)
  (let ((y (remainder (square x) m)))
    (if (and (= y 1) (!= x 1) (!= x (- m 1)))
        0
        y)))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (sqrmod (expmod base (/ exp 2) m) m))  
        (else (remainder (* base (expmod base (- exp 1) m)) m))))

(define (witness? a n)
  (!= (expmod a (- n 1) n) 1))

(define (miller-rabin-test n)
  (not (witness? (random-integer-below n) n)))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((miller-rabin-test n) (fast-prime? n (- times 1)))
        (else false)))

(fast-prime? 561 10)
(fast-prime? 7 10)
(fast-prime? 22 10)