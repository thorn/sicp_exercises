#lang racket

(define (square x) (* x x))
(define runtime current-milliseconds)

(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))
(define (divides? a b) (= (remainder a b) 0))

(define (next test-divisor)
  (if (= test-divisor 2)
      3
      (+ test-divisor 2)))


(define (prime? n)
  (= (smallest-divisor n) n))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (when (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes n count)
  (when (> count 0)
      (if (timed-prime-test n)
          (search-for-primes (+ n 1) (- count 1))
          (search-for-primes (+ n 1) count))))
(search-for-primes 100000000000 10) ; avg 30.5 => 7.5
(newline)
(search-for-primes 1000000000000 10) ; avg 88 => 33.8
(newline)
(search-for-primes 10000000000000 10) ; avg 251 => 104.3

; Yep, time went down around 2.5 times