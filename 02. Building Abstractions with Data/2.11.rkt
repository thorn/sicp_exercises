#lang racket
#|
In passing, Ben also cryptically comments: “By testing the signs of the endpoints of the intervals, it is possible to break mul-interval into nine cases, only one of which requires more than two multiplications.” Rewrite this procedure using Ben’s suggestion.

After debugging her program, Alyssa shows it to a potential user, who complains that her program solves the wrong problem. He wants a program that can deal with numbers represented as a centre value and an additive tolerance; for example, he wants to work with intervals such as 3.4 +- 0.15 rather than [3.35, 3.65]. Alyssa returns to her desk and fixes this problem by supplying an alternate constructor and alternate selectors:

(define (make-centre-width c w)
  (make-interval (- c w) (+ c w)))

(define (centre i)
  (/ (+ (lower-bound i)
        (upper-bound i))
     2))

(define (width i)
  (/ (- (upper-bound i)
        (lower-bound i))
     2))

Unfortunately, most of Alyssa’s users are engineers. Real engineering situations usually involve measurements with only a small uncertainty, measured as the ratio of the width of the interval to the midpoint of the interval. Engineers usually specify percentage tolerances on the parameters of devices, as in the resistor specifications given earlier.
|#


(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
(define (mul-interval x y)
  (let ((x0 (lower-bound x))
        (x1 (upper-bound x))
        (y0 (lower-bound y))
        (y1 (upper-bound y)))
    (cond ((is-positive? x)
           (cond ((is-positive? y) (make-interval (* x0 y0) (* x1 y1)))
                 ((is-negative? y) (make-interval (* x1 y0) (* x0 y1)))
                 ((spans-zero? y) (make-interval (* x1 y0) (* x1 y1)))))
          ((is-negative? x)
           (cond ((is-positive? y) (make-interval (* x0 y1) (* x1 y0)))
                 ((is-negative? y) (make-interval (* x1 y1) (* x0 y0)))
                 ((spans-zero? y) (make-interval (* x0 y1) (* x0 y0)))))
          ((spans-zero? x)
           (cond ((is-positive? y) (make-interval (* x0 y1) (* x1 y1)))
                 ((is-negative? y) (make-interval (* x1 y0) (* x0 y0)))
                 ((spans-zero? y) (make-interval
                                   (min (* x0 y0) (* x0 y1) (* x1 y0) (* x1 y1))
                                   (max (* x0 y0) (* x0 y1) (* x1 y0) (* x1 y1)))))))))

(define (div-interval x y)
  (if (spans-zero? y)
      (error "ERROR: Interval spans zero")
      (mul-interval
       x
       (make-interval (/ 1.0 (upper-bound y))
                      (/ 1.0 (lower-bound y))))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y))))

(define (make-interval a b) (cons a b))
(define (lower-bound i) (min (car i) (cdr i)))
(define (upper-bound i) (max (car i) (cdr i)))

(define (get-width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (spans-zero? i) (<= (* (lower-bound i) (upper-bound i)) 0))

(define (make-centre-width c w)
  (make-interval (- c w) (+ c w)))

(define (centre i)
  (/ (+ (lower-bound i)
        (upper-bound i))
     2))

(define (width i)
  (/ (- (upper-bound i)
        (lower-bound i))
     2))

(define (is-positive? i)
  (and (>= 0 (upper-bound i))
       (>= 0 (lower-bound i))))
(define (is-negative? i)
  (not (is-positive? i)))
; Test

(define i1 (make-interval 1 2))
(define i2 (make-interval -1 -2))
(define i3 (make-interval 1 -2))

(mul-interval i1 i1)
(mul-interval i1 i2)
(mul-interval i1 i3)
(mul-interval i2 i1)
(mul-interval i2 i2)
(mul-interval i2 i3)
(mul-interval i3 i1)
(mul-interval i3 i2)
(mul-interval i3 i3)