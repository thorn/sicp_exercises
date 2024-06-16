#lang racket

#|
Modify the timed-prime-test procedure of Exercise 1.22 to use fast-prime? (the Fermat method), and test each of the 12 primes you found in that exercise. Since the Fermat test has Θ(log n) growth, how would you expect the time to test primes near 1,000,000 to compare with the time needed to test primes near 1000? Do your data bear this out? Can you explain any discrepancy you find?
|#

(define max-random-value 4294967087)

(define (large-random upper-bound)
  (if (<= upper-bound max-random-value)
      (random upper-bound)
      (let* ((high (random (quotient upper-bound max-random-value)))
             (low (random max-random-value)))
        (+ (* high max-random-value) low))))

(large-random 10000000000)

(define (runtime) (current-inexact-milliseconds))

(define (next-divisor test-divisor)
  (if (= test-divisor 2) 3
        (+ test-divisor 2)))

(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next-divisor test-divisor)))))
(define (divides? a b) (= (remainder b a) 0))

(define (square x) (* x x))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder
          (* base (expmod base (- exp 1) m))
          m))))
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (large-random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n)(fast-prime? n (- times 1)))
        (else false)))

(define (prime? n) (fast-prime? n 10000))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (cond ((prime? n)
      (report-prime (- (runtime) start-time)))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes number-from prime-count)
  (when (> prime-count 0) 
    (cond ((and (odd? number-from) (prime? number-from))
           (timed-prime-test number-from)
           (search-for-primes (+ number-from 1) (- prime-count 1)))
          (else (search-for-primes (+ number-from 1) prime-count)))))

(search-for-primes 1000000000 3)
(search-for-primes 10000000000 3)
(search-for-primes 100000000000 3)
(search-for-primes 1000000000000 3)
(search-for-primes 10000000000000 3)
(search-for-primes 100000000000000 3)
(search-for-primes 1000000000000000 3)

#|
1000000007 *** 58.546875
1000000009 *** 67.65380859375
1000000021 *** 54.37109375
10000000019 *** 95.326904296875
10000000033 *** 83.656982421875
10000000061 *** 101.974853515625
100000000003 *** 106.137939453125
100000000019 *** 98.841796875
100000000057 *** 107.2412109375
1000000000039 *** 107.068115234375
1000000000061 *** 108.677978515625
1000000000063 *** 119.6337890625
10000000000037 *** 119.656005859375
10000000000051 *** 126.495849609375
10000000000099 *** 126.015869140625
100000000000031 *** 135.3310546875
100000000000067 *** 129.676025390625
100000000000097 *** 134.712158203125
1000000000000037 *** 156.80908203125
1000000000000091 *** 160.804931640625
1000000000000159 *** 106.025146484375

The complexity growth is very small
|#