#lang racket
#|
Demonstrate that the Carmichael numbers listed in Footnote 1.47 really do fool the Fermat test. That is, write a procedure that takes an integer n and tests whether a**n is congruent to a modulo n for every a < n, and try your procedure on the given Carmichael numbers.
|#

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder
          (* base (expmod base (- exp 1) m))
          m))))
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (large-random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n)(fast-prime? n (- times 1)))
        (else false)))

(define (square n) (* n n))
(define max-random-value 4294967087)
(define (large-random upper-bound)
  (if (<= upper-bound max-random-value)
      (random upper-bound)
      (let* ((high (random (quotient upper-bound max-random-value)))
             (low (random max-random-value)))
        (+ (* high max-random-value) low))))

; The smallest few are 561, 1105, 1729, 2465, 2821, and 6601.

(define (carmichael-number? n)
  (define (try-it n a)
    (cond ((= a n) #t)
          ((not (= (expmod a n n) a)) #f)
          (else (try-it n (+ a 1)))))
  (try-it n 1))

(carmichael-number? 561)
(carmichael-number? 1105)
(carmichael-number? 1729)
(carmichael-number? 2465)
(carmichael-number? 2821)
(carmichael-number? 6601)
(carmichael-number? 6602)