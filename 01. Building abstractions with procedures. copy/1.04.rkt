#lang racket
; Observe that our model of evaluation allows for combinations whose operators are compound expressions. Use this observation to describe the behavior of the following procedure:
(define (a-plus-abs-b a b) ((if (> b 0) + -) a b))

; here the (if (> b 0) + -)) part unfolds just to + or -. These are methods that are called by the preceeding bracket