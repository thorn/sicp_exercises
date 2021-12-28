#lang racket
(define call-the-cops "THE COPS ARE ON THEIR WAY HERE!")

(define (make-account balance password)
  (define (withdraw amount)          
    (if (>= balance amount)
        (begin (set! balance (- balance amount)) balance)
        "Insufficient funds"))

  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

  (define failed-attempts 0)

  (define (reset-failed-attempts) (set! failed-attempts 0))

  (define (dispatch pass m)
    (cond ((not (eq? pass password))
           (begin
             (set! failed-attempts (+ failed-attempts 1))
             (if (> failed-attempts 7)
                 (lambda (arg) (display call-the-cops))
                 (lambda (arg) "Wrong password")))
           )
          
          ((eq? m 'withdraw) (begin
           (reset-failed-attempts)
           withdraw))

          ((eq? m 'deposit)
           reset-failed-attempts
           deposit)
          (else (error "Unknown call -- MAKE-ACCOUNT" m))))
  dispatch)

(define acc (make-account 10 'my-pass))
((acc 'my-pass 'withdraw) 5)
((acc 'my-1pass 'withdraw) 5)
((acc 'my-1pass 'withdraw) 5)
((acc 'my-pass 'withdraw) 5) ; correct password
((acc 'my-1pass 'withdraw) 5) ; 1
((acc 'my-1pass 'withdraw) 5) ; 2
((acc 'my-1pass 'withdraw) 5) ; 3
((acc 'my-1pass 'withdraw) 5) ; 4
((acc 'my-1pass 'withdraw) 5) ; 5
((acc 'my-1pass 'withdraw) 5) ; 6
((acc 'my-1pass 'withdraw) 5) ; 7
((acc 'my-1pass 'withdraw) 5) ; call the cops!
