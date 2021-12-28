#lang sicp

(define (has-cycle? xs)
  (define (seen-last-pair? x)
    (or (null? x) (null? (cdr x))))
  (define (chase turtle rabbit)
    (cond ((or (null? turtle) (null? rabbit)) #f)
           ((eq? turtle rabbit) #t)
           ((seen-last-pair? (cdr rabbit)) #f)
           (else (chase (cdr turtle) (cddr rabbit)))))
  (if (seen-last-pair? xs)
      #f
      (chase xs (cdr xs))))
 
(define l1 (list 'a 'b 'c))
(define l2 (list 'a 'b 'c))
(set-cdr! (cdr (cdr l2)) l2)
(define l3 (list 'a 'b 'c 'd 'e))
(set-cdr! (cdddr l3) (cdr l3))
(define l4 (list 'a 'b 'c 'd 'e))
(set-car! (cdddr l4) (cddr l4))
 
(has-cycle? l1)
(has-cycle? l2)
(has-cycle? l3)
(has-cycle? l4)
(has-cycle? '(1 1))