#lang racket
(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (next a)
  (if (= a 2) 3
      (+ a 2)))

(define (divides? a b) (= (remainder b a) 0))
(define (square a) (* a a))

(define (prime? n) (= n (smallest-divisor n)))

(define (runtime) (current-milliseconds))

(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime n (- (runtime) start-time))
      false))

(define (report-prime n elapsed-time)
  (display n)
  (display " *** ")
  (display elapsed-time)
  (newline))
  

(define (search-for-primes number-from prime-count) 
  (when (> prime-count 0) 
      (if (timed-prime-test number-from) 
          (search-for-primes (+ number-from 1) (- prime-count 1))
          (search-for-primes (+ number-from 1) prime-count))))

(search-for-primes 100000000000 3)
