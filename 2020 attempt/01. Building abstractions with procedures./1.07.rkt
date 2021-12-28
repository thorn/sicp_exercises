;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |1.7|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define (square x) (* x x))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2.0))

(define (sqrt-iter prev-guess guess x)
  (if (better-good-enough? prev-guess guess)
      guess
      (sqrt-iter guess (improve guess x) x)))

(define (better-good-enough? prev-guess guess)
  (< (abs (/ (- guess prev-guess) prev-guess)) 0.01))

(define (my-sqrt x)
  (sqrt-iter 1.0 2.0 x))

(my-sqrt 100000)