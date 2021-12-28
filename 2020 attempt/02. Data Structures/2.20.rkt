#lang racket

(define (same-parity original . items)
  (define (same-as-original-parity? a) (even? (+ original a)))
  (define (filter-parity values)
    (cond ((null? values) null)
          ((same-as-original-parity? (car values)) (cons (car values) (filter-parity (cdr values))))
          (else (filter-parity (cdr values)))))
  (cons original (filter-parity items)))


(same-parity 1 2 3 4 5 6 7)
;(1 3 5 7)
(same-parity 2 3 4 5 6 7)
;(2 4 6)
