#lang racket

(define random-init 0) 
(define (rand-update x) (+ x 1))

(define rand
  (let ((x random-init))
    (define (dispatch message)
      (cond ((eq? message 'generate) (set! x (rand-update x)) x)
            ((eq? message 'reset) (lambda (new-initial) (set! x new-initial) x))
            (else (error "Unknown message -- RAND" message))))
    dispatch))

(rand 'generate)
; 1 
(rand 'generate)
; 2 
((rand 'reset) 0)
; 0 
(rand 'generate)
; 1
