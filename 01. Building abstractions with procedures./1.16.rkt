#lang racket
; Design a procedure that evolves an iterative exponentiation
; process that uses successive squaring and uses a logarithmic
; number of steps, as does fast-expt.

(define (even? x) (= (remainder x 2) 0))

(define (fast-exp-i base n)
  (define (fast-exp-iter a b n)
    (cond ((= n 0) a)
          ((even? n) (fast-exp-iter a (* b b) (/ n 2)))
          (else (fast-exp-iter (* a b) b (- n 1)))))

  (fast-exp-iter 1 base n))

(fast-exp-i 2 8)
(fast-exp-i 2 9)
(fast-exp-i 2 10)