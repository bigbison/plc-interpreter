#lang racket
(require "simpleParser.rkt")


; Implement  +,-,*,/,% as well as ==, !=,<,>,<=,>=, and &&,||,!.
; Variables may be true, false or an integer


;We need to find a way to return a number if the expression is simply a number


(define operator car)
(define operand1 cadr)
(define operand2 caddr)


(define M_value
  (lambda (expression state)
    (cond
      ((null? expression) (error 'parser "parser should have caught this"))
      ((number? expression) expression)
      ((eq? '+ (car expression)) (+ (M_value(car (cdr expression)) '()) (M_value (car (cdr (cdr expression))) '() )))
      ((eq? '* (car expression)) (* (M_value(car (cdr expression)) '()) (M_value (car (cdr (cdr expression))) '() )))
      ((eq? '- (car expression)) (- (M_value(car (cdr expression)) '()) (M_value (car (cdr (cdr expression))) '() )))
      ((eq? '/ (car expression)) (quotient (M_value(car (cdr expression)) '()) (M_value (car (cdr (cdr expression))) '() )))
      ((eq? '% (car expression)) (modulo (M_value(car (cdr expression)) '()) (M_value (car (cdr (cdr expression))) '() )))
      )))



(define declare
  (lambda (input state)
    (cond
      ((null? state) (declare input '(()()) ))
      ((null? input) state)
      ((number? (car input)) (declare (cdr input) (cons (car state) (cons (list(car input)) (car(cdr state))))))
      )))

(define retrieve
  (lambda (input state)
    (cond
      ((null? state) null)
      ((null? input) null)
      ((eq? input (caar state)) (caadr state))
      (else (M_retrieve input (rebuild (cdr (car state)) (cdr (cadr state)))))
      )))

; checks if our statement is a return statement
(define return?
  (lambda (stmt)
    (cond
      ((eq? 'return (car statement)) #t)
      (else #f))))

; checks if our statement is an assign statement
(define assignment?
  (lambda (statement)
    (cond
      ((eq? '= (operator statement)) #t)
      (else #f))))

; checks if we have an if else statement
; must be called prior to the plain if statement
(define if-else?
  (lambda (statement)
    (cond
      ((null? (caddr statement)) #f) ; no else
      ((and (eq? 'if (car statement)) (eq? 'else (caddr statement))) #t)
      (else #f))))

; checks if we have an if statement by itself
(define if?
  (lambda (statement)
    (cond
      ((eq? 'if (car statement)) #t)
      (else #f))))

; checks if we have a while statement
(define while?
  (lambda (statement)
    (cond
      ((eq? 'while (car statement)) #t)
      (else #f))))

; chekcs if we have a simple declare statement
(define declare?
  (lambda (statement)
    (cond
      ((eq? 'var (car statement)) #t)
      (else #f))))

; checks if we have a declaration and assignment in the same statement
; TODO check if this handles this statement correctly
(define declare-assignment?
  (lambda (statement)
    (cond
      ((and (eq? 'var (car statement)) (not (null? (caddr statement)))) #t)
      (else #f))))

; 
  


; Adds values and assoicated variables to the state list
(define declare*
  (lambda (vars vals state)
    (cond
      [(null? state) (declare* vars vals '(()()))] ;if the statelist is not implemented, sets up an empty statelist
      [(null? vars) state] ;if we have added all vars, return statelist
      ;if we are out of values, add the remaining vars to the statelist with a null value
      [(null? vals) (declare* (cdr vars) vals (rebuild (cons (car vars) (car state)) (cons '() (cadr state))))]
      ;else, add the var and it's associated values to the statelist.
      [else (declare* (cdr vars) (cdr vals) (rebuild (cons (car vars) (car state)) (cons (car vals) (cadr state))))]
    )
  )
  )



(