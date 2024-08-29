#lang racket

#|
The width of an interval is half of the difference between its upper and lower bounds. The width is a measure of the uncertainty of the number specified by the interval. For some arithmetic operations the width of the result of combining two intervals is a function only of the widths of the argument intervals, whereas for others the width of the combination is not a function of the widths of the argument intervals. Show that the width of the sum (or difference) of two intervals is a function only of the widths of the intervals being added (or subtracted). Give examples to show that this is not true for multiplication or division.
|#


(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y) (mul-interval
   x
   (make-interval (/ 1.0 (upper-bound y))
                  (/ 1.0 (lower-bound y)))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y))))

(define (make-interval a b) (cons a b))
(define (lower-bound i) (min (car i) (cdr i)))
(define (upper-bound i) (max (car i) (cdr i)))

(define (get-width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define i1 (make-interval 3 11))
(define i2 (make-interval 7 2))

(display "sum: ") (get-width (add-interval i1 i2))
(display "diff: ")(get-width (sub-interval i1 i2))
(newline)
(display "widths added: ") (+ (get-width i1) (get-width i2))



; Doesn't work for multiplication
(define i3 (make-interval 3 11))
(define i4 (make-interval 7 2))

(display "mul: ") (get-width (mul-interval i3 i4))
(display "div: ") (get-width (div-interval i3 i4))
(newline)
(display "widths added: ") (+ (get-width i3) (get-width i4))