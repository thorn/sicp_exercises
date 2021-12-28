#lang racket

(define (make-from-mac-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* r (cos a)))
          ((eq? op 'imag-part)  (* r (sin a)))
          ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          (else
           (error "Unknown operation -- MAKE-FROM-MAC-ANG" op))))
  dispatch)