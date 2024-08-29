#lang racket
#|
A binary mobile consists of two branches, a left branch and a right branch. Each branch is a rod of a certain length, from which hangs either a weight or another binary mobile. We can represent a binary mobile using compound data by constructing it from two branches (for example, using list):

(define (make-mobile left right)
  (list left right))

A branch is constructed from a length (which must be a number) together with a structure, which may be either a number (representing a simple weight) or another mobile:

(define (make-branch length structure)
  (list length structure))

a.  Write the corresponding selectors left-branch and right-branch, which return the branches of a mobile, and branch-length and branch-structure, which return the components of a branch.

b.  Using your selectors, define a procedure total-weight that returns the total weight of a mobile.

c.  A mobile is said to be balanced if the torque applied by its top-left branch is equal to that applied by its top-right branch (that is, if the length of the left rod multiplied by the weight hanging from that rod is equal to the corresponding product for the right side) and if each of the submobiles hanging off its branches is balanced. Design a predicate that tests whether a binary mobile is balanced.

d.  Suppose we change the representation of mobiles so that the constructors are

(define (make-mobile left right)
  (cons left right))
(define (make-branch length structure)
  (cons length structure))

How much do you need to change your programs to convert to the new representation?
|#


(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

; a)
(define left-branch car)
(define right-branch cadr)
(define branch-length car)
(define branch-structure cadr)

; b)
(define (total-weight m)
  (if (not (pair? m))
      m
      (+ (total-weight (branch-structure (left-branch m)))
         (total-weight (branch-structure (right-branch m))))))

; c)
(define (torque branch)
  (* (branch-length branch) (total-weight (branch-structure branch))))

(define (balanced? m)
  (if (not (pair? m))
      true
      (and (= (torque (left-branch m)) (torque (right-branch m)))
              (balanced? (branch-structure (left-branch m)))
              (balanced? (branch-structure (right-branch m))))))

; d)
#|
Suppose we replace list with cons in the following methods:
(define (make-mobile left right)
  (cons left right)) ; was list

(define (make-branch length structure)
  (cons length structure)) ; was list

Then all the changes are:
(define left-branch car)
(define right-branch cdr) ; was cadr
(define branch-length car)
(define branch-structure cdr) ; was cadr
|#

(define a (make-mobile
           (make-branch
            2
            (make-mobile (make-branch 3 4) (make-branch 4 5)))
           (make-branch 2 3)))
(total-weight a) ;; 4 + 5 + 3 = 12

(define b (make-mobile 
            (make-branch 4 6) 
            (make-branch 5 
                         (make-mobile 
                          (make-branch 3 7) 
                          (make-branch 9 8))))) 
(total-weight b) ;; 6 + 7 + 8 = 21

(define c (make-mobile (make-branch 10 (make-mobile (make-branch 2 3) (make-branch 2 3))) (make-branch 12 5))) 
(balanced? a) ; #f
(balanced? b) ; #f
(balanced? c) ; #t