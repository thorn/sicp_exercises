#lang racket

; Процедура accumulate-n подобна accumulate, только свой третий аргумент она восприни-
; мает как последовательность последовательностей, причем предполагается, что все они содержат
; одинаковое количество элементов. Она применяет указанную процедуру накопления ко всем пер-
; вым элементам последовательностей, вторым элементам последовательностей и так далее, и воз-
; вращает последовательность результатов. Например, если s есть последовательность, состоящая
; из четырех последовательностей, ((1 2 3) (4 5 6) (7 8 9) (10 11 12)), то значением
; (accumulate-n + 0 s) будет последовательность (22 26 30). Заполните пробелы в следую-
; щем определении accumulate-n:


(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      null
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define s (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))

(accumulate-n + 0 s)