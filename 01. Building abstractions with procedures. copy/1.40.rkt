#lang racket

#|
Define a procedure cubic that can be used together with the newtons-method procedure in expressions of the form
      (newtons-method (cubic a b c) 1)

to approximate zeros of the cubic x^3 + ax^2 + bx + c.
|#

(define (average x y) (+ (/ x 2) (/ y 2)))

(define (fixed-point f first-guess)
  (define tolerance 0.0000001)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (deriv g)
  (define dx 0.00001)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (cubic a b c)
  (lambda (x) (+ (* x x x) (* a x x) (* b x) c)))

(define (solve a b c)
  (newtons-method (cubic a b c) 1))
(solve 1 1 1)