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
