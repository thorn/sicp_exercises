#lang racket

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (fold-right op init seq)
  (accumulate op init seq))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (reverse sequence)
(fold-right (lambda (x y) (append y (list x))) null sequence))

(define (reverse-left sequence)
(fold-left (lambda (x y) (cons y x)) null sequence))

(define my-list (list 1 2 3 4))
(reverse my-list)
(reverse-left my-list)