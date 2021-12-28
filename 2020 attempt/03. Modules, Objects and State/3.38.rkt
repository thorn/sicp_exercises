#lang racket
(define f 
   (let ((count 1)) 
     (lambda (x)  
        (set! count (* count x)) 
        count)))
(+ (f 0) (f 1))
(+ (f 1) (f 0))
