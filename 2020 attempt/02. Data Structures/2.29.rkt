#lang racket

; Бинарный мобиль состоит из двух ветвей, левой и правой. Каждая ветвь представляет собой
; стержень определенной длины, с которого свисает либо гирька, либо еще один бинарный мобиль.
; Мы можем представить бинарный мобиль в виде составных данных, соединив две ветви.
; Ветвь составляется из длины length (которая должна быть числом) и структуры structure,
; которая может быть либо числом (представляющим простую гирьку), либо еще одним мобилем

; а. Напишите соответствующие селекторы left-branch и right-branch, которые возвраща-
; ют левую и правую ветви мобиля, а также branch-length и branch-structure, которые
; возвращают компоненты ветви.

(define (make-mobile left right) (list left right))

(define (make-branch length structure) (list length structure))

(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (cadr mobile))
(define (branch-length branch) (car branch))
(define (branch-structure branch) (cadr branch))

; б. С помощью этих селекторов напишите процедуру total-weight, которая возвращает общий
; вес мобиля.

(define (total-weight mobile) 
  (+ (branch-weight (left-branch mobile)) 
     (branch-weight (right-branch mobile))))

(define (branch-weight branch)
    (if (pair? (branch-structure branch)) 
        (total-weight (branch-structure branch)) 
        (branch-structure branch)))

(define submobile (make-mobile 20 1))
(define my-left-branch (make-branch 10 submobile))

(define my-mobile (make-mobile (make-branch 10 (make-mobile (make-branch 10 1)
                                               (make-branch 20 1)))
                               (make-branch 40 2)))

; в. Говорят, что мобиль сбалансирован, если момент вращения, действующий на его левую ветвь,
; равен моменту вращения, действующему на правую ветвь (то есть длина левого стержня, умножен-
; ная на вес груза, свисающего с него, равна соответствующему произведению для правой стороны),
; и если все подмобили, свисающие с его ветвей, также сбалансированы. Напишите предикат, кото-
; рый проверяет мобили на сбалансированность.

(define (mobile-balanced? mobile)
  (= (branch-balance (left-branch mobile))
     (branch-balance (right-branch mobile))))

(define (total-length mobile)
  (+ (branch-length (left-branch mobile))
     (branch-length (right-branch mobile))))

(define (length-branch branch)
  (if (pair? (branch-structure branch))
      (+ (branch-length branch) (total-length (branch-structure branch)))
      (branch-length branch)))

(define (branch-balance branch)
  (* (branch-weight branch) (length-branch branch)))

(define (balanced? mobile)
  (define (branch-balanced? branch)
    (if (pair? (branch-structure branch))
        (balanced? (branch-structure branch))
        true))
  (define (torque branch)
    (* (branch-length branch) (branch-weight branch)))
  (let ((left (left-branch mobile))
        (right (right-branch mobile)))
    (and (branch-balanced? left)
         (branch-balanced? right)
         (= (torque left) (torque right)))))

(total-weight my-mobile)
(length-branch (left-branch my-mobile))
(length-branch (right-branch my-mobile))
(mobile-balanced? my-mobile)
(balanced? my-mobile)