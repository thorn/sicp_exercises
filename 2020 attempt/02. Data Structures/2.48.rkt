#lang sicp

(#%require sicp-pict)

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (make-frame2 origin edge1 edge2)
  (cons origin edge1 edge2))

(define origin-frame car)
(define (edge1-frame f) (cadr f))
(define (edge2-frame f) (caddr f))

(define origin-frame2 car)
(define (edge1-frame2 f) (cadr f))
(define (edge2-frame2 f) (cddr f))


(define make-vect cons)
(define xcor-vect car)
(define ycor-vect cdr)

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
             (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect v x)
  (make-vect (* (xcor-vect v) x)
             (* (ycor-vect v) x)))

(define f (make-frame (cons 0 0) (make-vect (list 0 0) (list 0 1)) (make-vect (list 0 0) (list 1 0))))
(origin-frame f)
(edge1-frame f)
(edge2-frame f)

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))

(define (make-segment start end)
  (cons start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))