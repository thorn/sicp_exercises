;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |1.8|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define (improve guess x) (/ (+ (/ x (square guess)) (* 2 guess)) 3))
(define (square x) (* x x))

(define (good-enough? old-guess new-guess)
  (< (abs (- old-guess new-guess)) 0.001))

(define (sqrt-iter old-guess new-guess x)
  (if (good-enough? old-guess new-guess)
      old-guess
      (sqrt-iter new-guess (improve new-guess x)
                 x)))
(define (cube-root x)
  (sqrt-iter 1.0 2.0 x))

(cube-root 27)
(cube-root 8)