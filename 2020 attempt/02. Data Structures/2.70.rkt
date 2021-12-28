#lang racket

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "BAD BIT -- CHOOSE-BRANCH" bit))))

(define (element-of-set? el set)
  (cond ((null? set) false)
        ((eq? el (car set)) true)
        (else (element-of-set? el (cdr set)))))

(define (encode-symbol char tree)
  (if (leaf? tree)
      null
      (cond ((null? tree) null)
            ((element-of-set? char (symbols (left-branch tree)))
             (cons 0 (encode-symbol char (left-branch tree))))
            ((element-of-set? char (symbols (right-branch tree)))
             (cons 1 (encode-symbol char (right-branch tree))))
            (else (error "No such symbol in the tree -- ENCODE-SYMBOL" char)))))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge pairs)
  (if (null? (cdr pairs))
      (car pairs)
      (successive-merge (adjoin-set (make-code-tree (car pairs) 
                                                    (cadr pairs)) 
                                    (cddr pairs)))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair) (cadr pair))
                    (make-leaf-set (cdr pairs))))))

(define rock-songs-pairs (list (list 'A 2) (list 'BOOM 1) (list 'GET 2) (list 'JOB 2) (list 'NA 16)
                               (list 'SHA 3) (list 'YIP 9) (list 'WAH 1)))

(define rock-songs-tree (generate-huffman-tree rock-songs-pairs))
(encode '(GET A JOB
          SHA NA NA NA NA NA NA NA NA
          GET A JOB
          SHA NA NA NA NA NA NA NA NA
          WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
          SHA BOOM) rock-songs-tree)