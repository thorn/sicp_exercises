#lang racket

(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
  (if (not (> (abs angle) 0.1)) angle
  (p (sine (/ angle 3.0)))))

(sine 12.15)

#|
Function p is applied 5 times:
1. 12.15 / 3
2. 4.05 / 3
3. 1.39.. / 3
4. 0.449.. / 3
5. 0.15 / 3 => 0.04 # <- this is less than 0.1 so stop the recursion


Since the parameter is divided by 3 every call, the maximum recursion debth would be logarithmic, thus O(log(n))
|#