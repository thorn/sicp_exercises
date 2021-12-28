#lang sicp
(#%require "../shared/tables.rkt")

(define my-equal? eq?)
(define operation-table (make-table my-equal?))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(define (install-syntax)
  (put 'op 'quote text-of-quotation)
  (put 'op 'set! eval-assignment)
  (put 'op 'define eval-definition)
  (put 'op 'if eval-if)
  (put 'op 'and eval-and-predicates)
  (put 'op 'or eval-or-predicates)
  (put 'op 'lambda (lambda (x y)  
                     (make-procedure (lambda-parameters x) (lambda-body x) y)))
  (put 'op 'begin (lambda (x y)
                    (eval-sequence (begin-sequence x) y)))
  (put 'op 'cond (lambda (x y)
                   (evaln (cond->if x) y)))
  'done)

(install-syntax)

(define (eval expr env)  
  (cond ((self-evaluating? expr) expr)  
        ((variable? expr) (lookup-variable-value expr env))  
        ((get 'op (operator expr)) ((get 'op (operator expr)) expr env))  
        ((application? expr)   
         (apply (eval (operator expr) env)   
                (list-of-values (operands expr) env)))  
        (else (error "Unknown expression type -- EVAL" expr))))

(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           arguments
           (procedure-environment procedure))))
        (else
         (error "Unknown procedure type -- APPLY" procedure))))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        ((boolean? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))
    
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (assignment? exp)
  (tagged-list? exp 'set))

(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

(define (definition? exp)
  (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exp seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operands ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (cond? exp) (tagged-list? exp 'cond))
(deifne (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if  
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "Else branch is not the last -- COND->IF" clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

(define (and? exp) (tagged-list? exp 'and))
(define (or? exp) (tagged-list? exp 'or))
(define boolean-predicates cdr)

(define (eval-and-predicates exps env)
  (cond ((no-predicates? exps) true)
        ((not (true? (eval (first-predicate exps)))) false)
        (else (eval-and-predicate (boolean-predicates exps) env))))

(define (eval-or-predicates exps env)
  (cond ((no-predicates? exps) false)
        ((true? (eval (first-predicate exps))) true)
        (else (eval-or-predicate (boolean-predicates exps) env))))

