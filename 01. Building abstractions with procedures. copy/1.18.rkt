#lang racket
#|
The exponentiation algorithms in this section are based on performing exponentiation by means of repeated multiplication. In a similar way, one can perform integer multiplication by means of repeated addition. The following multiplication procedure (in which it is assumed that our language can only add, not multiply) is analogous to the expt procedure:

(define (* a b) (if (= b 0)
      0
      (+ a (* a (- b 1)))))

This algorithm takes a number of steps that is linear in b. Now suppose we include, together with addition, operations double, which doubles an integer, and halve, which divides an (even) integer by 2. Using these, design a multiplication procedure analogous to fast-exp that uses a logarithmic number of steps.
|#

(require rackunit)
(define (fast-mul b n)
  (define (fast-mul-iter b n a)
    (cond ((= n 0) a)
          ((even? n) (fast-mul-iter (double b) (halve n) a))
          (else  (fast-mul-iter b (- n 1) (+ a b)))))

  (define (even? x) (= (remainder x 2) 0))
  (define (double x) (+ x x))
  (define (halve x) (/ x 2))
  (fast-mul-iter b n 0))

(check-eq? (fast-mul 2 9) 18)
(check-eq? (fast-mul 2 10) 20)
(check-eq? (fast-mul 2 11) 22)
(check-eq? (fast-mul 2 12) 24)
