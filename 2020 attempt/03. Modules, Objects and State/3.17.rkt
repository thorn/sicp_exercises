#lang sicp

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

(define x1 '(a b c))
x1
(count-pairs x1)

(define x '(a))
(define xv (cons x x))
(define x2 (list xv))
x2
(count-pairs x2)

(define x3 (cons xv xv))
x3
(count-pairs x3)

(define endless '(a b c))
(set-cdr! (cddr endless) endless)
; (count-pairs endless)
; maximum recursion depth exceeded

(define (my-count-pairs x)
  (let ((encountered '()))
    (define (helper x)
      (if (or (not (pair? x)) (memq x encountered))
          0
          (begin
            (set! encountered (cons x encountered))
            (+ (helper (car x))
               (helper (cdr x))
               1)))
      )
    (helper x)))
(my-count-pairs x1)
(my-count-pairs x2)
(my-count-pairs x3)