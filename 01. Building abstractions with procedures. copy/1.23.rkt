#lang racket
#|
The smallest-divisor procedure shown at the start of this section does lots of needless testing: After it checks to see if the number is divisible by 2 there is no point in checking to see if it is divisible by any larger even numbers. This suggests that the values used for test-divisor should not be 2, 3, 4, 5, 6, . . ., but rather 2, 3, 5, 7, 9, . . ..
To implement this change, define a procedure next that returns 3 if its input is equal to 2 and otherwise returns its input plus 2. Modify the smallest-divisor procedure to use (next test-divisor) instead of (+ test-divisor 1). With timed-prime-test incorporating this modified version of smallest-divisor, run the test for each of the 12 primes found in Exercise 1.22. Since this modification halves the number of test steps, you should expect it to run about twice as fast. Is this expectation confirmed? If not, what is the observed ratio of the speeds of the two algorithms, and how do you explain the fact that it is different from 2?
|#

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

Before optimization				After optimization				Ratio after/before
1,000,000,007		6.720947266		1,000,000,007		0.7990722656		-741.09%
1,000,000,009		2.714111328		1,000,000,009		4.557128906		40.44%
1,000,000,021		2.870117188		1,000,000,021		1.927001953		-48.94%
10,000,000,019		5.274902344		10,000,000,019		7.896972656		33.20%
10,000,000,033		4.390869141		10,000,000,033		3.46484375		-26.73%
10,000,000,061		7.270019531		10,000,000,061		4.036865234		-80.09%
100,000,000,003		16.75317383		100,000,000,003		10.80786133		-55.01%
100,000,000,019		20.19287109		100,000,000,019		21.5769043		6.41%
100,000,000,057		15.55615234		100,000,000,057		23.32495117		33.31%
1,000,000,000,039	54.4699707		1,000,000,000,039	75.07202148		27.44%
1,000,000,000,061	56.46704102		1,000,000,000,061	89.85986328		37.16%
1,000,000,000,063	52.67114258		1,000,000,000,063	35.48120117		-48.45%
10,000,000,000,037	194.0510254		10,000,000,000,037	119.5168457		-62.36%
10,000,000,000,051	195.1181641		10,000,000,000,051	129.0720215		-51.17%
10,000,000,000,099	192.8640137		10,000,000,000,099	122.0258789		-58.05%
100,000,000,000,031	603.7600098		100,000,000,000,031	377.045166		-60.13%
100,000,000,000,067	601.4289551		100,000,000,000,067	387.0119629		-55.40%
100,000,000,000,097	612.6630859		100,000,000,000,097	602.625			-1.67%
1,000,000,000,000,030	1922.790039		1,000,000,000,000,030	1446.832031		-32.90%
1,000,000,000,000,090	1896.99707		1,000,000,000,000,090	1283.958008		-47.75%
1,000,000,000,000,150	1854.954102		1,000,000,000,000,150	1283.819092		-44.49%

There're no 200% imprevement anywhere

Inlining the calculation of the next divisors brought up to 100% improvement
|#