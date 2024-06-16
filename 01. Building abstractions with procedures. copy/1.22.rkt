#lang racket
#|
Most Lisp implementations include a primitive called runtime that returns an integer that specifies the amount of time the system has been running (measured, for example, in microseconds). The following timed-prime-test procedure, when called with an integer n, prints n and checks to see if n is prime. If n is prime, the procedure prints three asterisks followed by the amount of time used in performing the test.

Using this procedure, write a procedure search-for-primes that checks the primality of consecutive odd integers in a specified range. Use your procedure to find the three smallest primes larger than 1000; larger than 10,000; larger than 100,000; larger than 1,000,000. Note the time needed to test each prime. Since the testing algorithm has order of growth of Θ(sqrt(n)), you should expect that testing for primes around 10,000 should take about sqrt(10) times as long as testing for primes around 1000. Do you timinig data bear this out? How well do the data for 100,000 and 1,000,000 support the Θ(sqrt(n)) prediction? Is your result compatible with the notion that programs on your machine run in time proportional to the number of steps required for the computation?
|#

(define (runtime) (current-inexact-milliseconds))


(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b) (= (remainder b a) 0))

(define (square x) (* x x))
(define (prime? n) (= (smallest-divisor n) n))
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

; find the three smallest primes larger than 1000

(define (odd? n) (= (remainder n 2) 1))

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
The results on my machine:

N                | Milliseconds  | Ratio
-----------------|---------------|------
1000000000       | 1             | 4.45
10000000000      | 4.45          | 5.91
100000000000     | 26.3          | 1.84
1000000000000    | 48.3          | 3.19
10000000000000   | 154.3         | 3.02
100000000000000  | 466           | 3.14
1000000000000000 | 1465          |

The expected ratio of sqrt(10) is ~3.162 and the ratio seems to hold
|#