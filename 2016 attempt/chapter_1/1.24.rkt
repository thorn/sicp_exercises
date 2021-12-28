;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |1.24|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require sicp)

(define (square x) (* x x))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
         m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))
        

(define (timed-prime-test n)
  (begin (newline)
         (display n)
         (start-prime-test n (runtime))))

(define (start-prime-test n start-time)
  (if (fast-prime? n 1)
      (report-prime (- (runtime) start-time))
      false))

(define (report-prime elapsed-time)
  (begin
    (display " *** ")
    (display elapsed-time)))

(define (prime-test start count)
  (if (<= count 0)
      (newline)
      (begin
        (if (timed-prime-test start)
            (prime-test (+ start 1) (- count 1))
            (prime-test (+ start 1) count))
        )))

(prime-test 1000 3)
(prime-test 10000 3)
(prime-test 100000 3)
(prime-test 1000000 3)