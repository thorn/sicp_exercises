#lang racket
(define (average a b) (/ (+ a b) 2))
(define (make-rectangle bottom-left top-right) (cons bottom-left top-right))
(define (bottom-left rectangle) (car rectangle))
(define (top-right rectangle) (cdr rectangle))
(define (make-point x y) (cons x y))
(define (x-point point) (car point))
(define (y-point point) (cdr point))
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define my-rectangle (make-rectangle (make-point 1 2) (make-point 5 5)))

(define (width rectangle) (- (x-point (top-right rectangle)) (x-point (bottom-left rectangle))))
(define (height rectangle) (- (y-point (top-right rectangle)) (y-point (bottom-left rectangle))))

(define (perimeter rectangle) (* (+ (width rectangle) (height rectangle)) 2))
(perimeter my-rectangle)