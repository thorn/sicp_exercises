#lang racket
#|
Alyssa P. Hacker complains that we went to a lot of extra work in writing expmod. After all, she says, since we already know how to compute exponentials, we could have simply written
(define (expmod base exp m) (remainder (fast-expt base exp) m))
Is she correct? Would this procedure serve as well for our fast prime tester? Explain.
|#

; This will first build up a huge number and then divide it in the end. The number is so huge it can cause overflow.