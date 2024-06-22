#lang racket
#|
We saw in Section 1.3.3 that attempting to compute square roots by naively finding a fixed point of y → x/y does not converge, and that this can be fixed by average damping. The same method works for finding cube roots as fixed points of the average-damped y → x/y^2. Unfortunately, the process does not work for fourth roots—a single average damp is not enough to make a fixed-point search for y → x/y^3 converge. On the other hand, if we average damp twice (i.e., use the average damp of the average damp of y → x/y^3) the fixed-point search does converge. Do some experiments to determine how many average damps are required to compute nth roots as a fixed-point search based upon repeated average damping of y → x/y^n−1. Use this to implement a simple procedure for computing nth roots using fixed-point, average-damp, and the repeated procedure of Exercise 1.43. Assume that any arithmetic operations you need are available as primitives.
|#


(define (compose f g) (lambda (x) (f (g x))))

(define (repeated f n)
  (if (= n 1)
      f
      (compose (repeated f (- n 1)) f)))

(define (power x n)
  (if (= n 1)
      x
      (* x (power x (- n 1)))))

(define (average x y) (+ (/ x 2) (/ y 2)))
(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))
(fixed-point cos 1.0)
(define (nth-root-damped x nth damp)
  (fixed-point
    ((repeated average-damp damp)
    (lambda (y)
      (/ x (power y (- nth 1)))))
   1.0))

(nth-root-damped 100 2 1)
(nth-root-damped 1000 3 1)
(nth-root-damped 10000 4 2)
(nth-root-damped 100000 5 2)
(nth-root-damped 1000000 6 2)
(nth-root-damped 10000000 7 2)
(nth-root-damped 100000000 8 3)
(nth-root-damped 1000000000 9 3)
(nth-root-damped 10000000000000000 16 4) ; 32 - 5, 64 - 6

(define (nth-root x nth)
  (fixed-point
     ((repeated average-damp (floor (log nth 2)))
     (lambda (y)
       (/ x (power y (- nth 1)))))
  1.0))
(nth-root 10000000000000000 16)