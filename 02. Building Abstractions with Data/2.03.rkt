#lang racket
#|
Implement a representation for rectangles in a plane. (Hint: You may want to make use of Exercise 2.2.) In terms of your constructors and selectors, create procedures that compute the perimeter and the area of a given rectangle. Now implement a different representation for rectangles. Can you design your system with suitable abstraction barriers, so that the same perimeter and area procedures will work using either representation?
|#

(define (square x) (* x x))

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (make-rectangle p1 p2) (cons p1 p2))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

(define (distance-point p1 p2)
  (sqrt
   (+ (square (- (x-point p1) (x-point p2)))
      (square (- (y-point p1) (y-point p2))))))


(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (rect-perimeter r)
  (* 2 (+ (rect-width r) (rect-height r))))
(define (rect-area r)
  (* (rect-width r) (rect-height r)))

; Helper
(define (dot-product p1 p2)
  (+ (* (x-point p1) (x-point p2))
     (* (y-point p1) (y-point p2))))

(define (add-vector v1 v2)
  (make-point (+ (x-point v1) (x-point v2))
              (+ (y-point v1) (y-point v2))))

(define (sub-vector v1 v2)
  (make-point (- (x-point v1) (x-point v2))
              (- (y-point v1) (y-point v2))))

(define (orthogonal? v1 v2)
  (= 0.0 (dot-product v1 v2))) ; should it be better with an small range?

; Constructor
(define (make-rect p1 p2 p3)
  (if (orthogonal? (sub-vector p2 p1) (sub-vector p3 p1))
      (cons p1 (cons p2 p3))
      (error "Points should make an rectangle."))) ; check orthogonality, which is longer? (error "Argument not 0 or 1: CONS" m)

(define (p1-rect r) (car  r))
(define (p2-rect r) (car (cdr r)))
(define (p3-rect r) (cdr (cdr r)))

; Public interface
(define (rect-height r) (distance-point (p1-rect r) (p2-rect r)))
(define (rect-width  r) (distance-point (p1-rect r) (p3-rect r)))

(define p4 (make-point 0 0))
(define p5 (make-point 10 -2))
(define p6 (make-point 1 5))

(define r2 (make-rect p4 p5 p6))

(display "Rectangle 2: ") (newline)
(display "Perimeter: ") (display (rect-perimeter r2)) (newline)
(display "Area ") (display (rect-area r2)) (newline) (newline)