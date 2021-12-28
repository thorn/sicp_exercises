#lang racket

(define (make-account balance password)
  (define (withdraw amount)          
    (if (>= balance amount)
        (begin (set! balance (- balance amount)) balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch pass m)
    (cond ((not (eq? pass password)) (lambda (arg) "Wrong password"))
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown call -- MAKE-ACCOUNT"
                       m))))
  dispatch)

(define acc (make-account 10 'my-pass))
((acc 'my-pass 'withdraw) 5)
((acc 'my-1pass 'withdraw) 5)