#lang racket
#|
Design a procedure that evolves an iterative exponentiation process that uses successive squaring and uses a logarithmic number of steps, as does fast-expt. (Hint: Using the observation that (bn/2)2 = (b2)n/2, keep, along with the exponent n and the base b, an additional state variable a, and define the state transformation in such a way that the product abn is unchanged from state to state. At the beginning of the process a is taken to be 1, and the answer is given by the value of a at the end of the process. In general, the technique of defining an invariant quantity that remains unchanged from state to state is a powerful way to think about the design of iterative algorithms.)
|#

(require rackunit)
(define (fast-exp b n)
  (define (fast-exp-iter b n a)
    (cond ((= n 0) a)
          ((even? n) (fast-exp-iter (square b) (/ n 2) a))
          (else  (fast-exp-iter b (- n 1) (* a b)))))

  (define (even? x) (= (remainder x 2) 0))
  (define (square x) (* x x))
  (fast-exp-iter b n 1))

(check-eq? (fast-exp 2 9) 512)
(check-eq? (fast-exp 2 10) 1024)
(check-eq? (fast-exp 2 11) 2048)
(check-eq? (fast-exp 2 12) 4096)
