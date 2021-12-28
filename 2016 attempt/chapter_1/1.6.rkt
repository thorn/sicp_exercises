;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |1.6|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define (new-if clause ok not-ok)
  (cond (clause ok)
        (else not-ok)))


(define (average x y) (/ (+ x y) 2))
(define (improve guess x) (average guess (/ x guess)))
(define (square x) (* x x))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt-iter guess x)
  ;; There would be stack overflow because of new-if is a function and
  ;; function params calculcated first before the even needed. So the
  ;; second param would make stack overflow because x won't ever change.
  (new-if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))
(define (square-root x)
  (sqrt-iter 1.0 x))

(square-root 9)
