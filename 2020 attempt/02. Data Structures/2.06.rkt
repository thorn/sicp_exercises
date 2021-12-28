#lang racket

(define zero (lambda(f) (lambda (x) x)))

(define one 
  (lambda (f) (lambda (x) (f x))))

(define two
  (lambda (f) (lambda (x) (f (f x)))))

(define (inc n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define (plus n m)
  (lambda (f) (lambda (x) ((m f) ((n f) x)))))

(plus one one)