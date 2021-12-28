#lang racket

(define (make-monitored f)
  (define times-called 0)
  (define (dispatch message)
    (cond ((eq? message 'how-many-calls?) times-called)
          ((eq? message 'reset-count) (set! times-called 0))
          (else 
           (set! times-called (+ times-called 1))
           (f message))))
  dispatch)
           

(define s (make-monitored sqrt))
(s 100)
(s 'how-many-calls?)
(s 16)
(s 'how-many-calls?)
(s 'reset-count)
(s 'how-many-calls?)