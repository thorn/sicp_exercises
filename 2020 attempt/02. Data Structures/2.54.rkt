#lang racket

(define (equal? a b)
  (or (and (null? a) (null? b))
      (and (number? a) (number? b) (eq? a b))
      (and (symbol? a) (symbol? b) (eq? a b))
      (and (pair? a) (pair? b) (equal? (car a) (car b)) (equal? (cdr a) (cdr b)))))

(equal? '(this is a list) '(this is a list))
(equal? '(this is a list) '(this (is a) list))
