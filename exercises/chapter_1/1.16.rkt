;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |1.16|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define (square x) (* x x))
(define (halve x) (/ x 2))

(define (fast-exp b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-exp b (/ n 2))))
        (else (* b (fast-exp b (- n 1))))))

(fast-exp 2 8)

(define (fast-exp-i b n)
  (fast-exp-iter 1 b n))

(define (fast-exp-iter acc b n)
  (cond ((= n 0) acc)
        ((even? n) (fast-exp-iter acc (square b) (halve n)))
        (else (fast-exp-iter (* acc b) b (- n 1)))))
(fast-exp-i 2 8)