#lang racket
#|
Demonstrate that Lem is right. Investigate the behavior of the system on a variety of arithmetic expressions. Make some intervals A and B, and use them in computing the expressions A/A and A/B. You will get the most insight by using intervals whose width is a small percentage of the center value. Examine the results of the computation in center-percent form (see Exercise 2.12).
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
(define (is-positive? i)
  (and (>= 0 (upper-bound i))
       (>= 0 (lower-bound i))))
(define (is-negative? i)
  (not (is-positive? i)))

(define (spans-zero? i) (<= (* (lower-bound i) (upper-bound i)) 0))

(define (par1 r1 r2)
(div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
(define (par2 r1 r2)
(let ((one (make-interval 1 1)))
    (div-interval
     one (add-interval (div-interval one r1)
                       (div-interval one r2)))))

(define a (make-interval 2 4)) 
(define b (make-interval 3 5))

; these functions output different results
(par1 a b)
(par2 a b)


(define (make-center-percent center percentage)
  (make-interval (- center (* (/ center 100) percentage))
                 (+ center (* (/ center 100) percentage))))

(println "div interval A/A)")
(define a1 (make-center-percent 2 5))
(define b1 (make-center-percent 3 5))
(div-interval a1 a1)
(div-interval a1 b1)