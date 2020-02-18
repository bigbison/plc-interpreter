#lang racket
(require "simpleParser.rkt")
(require "statefunction.rkt")


; Implement  +,-,*,/,% as well as ==, !=,<,>,<=,>=, and &&,||,!.
; Variables may be true, false or an integer

;if, if else, while, conditional

;We need to find a way to return a number if the expression is simply a number


(define operator car)
(define operand1 cadr)
(define operand2 caddr)


(define M_value
  (lambda (expression state)
    (cond
      ((null? expression) (error 'parser "parser should have caught this"))
      ((number? expression) expression)
      ((eq? '+ (car expression)) (+ (M_value(cadr expression) '()) (M_value (caddr expression) '() )))
      ((eq? '* (car expression)) (* (M_value(cadr expression) '()) (M_value (caddr expression) '() )))
      ((eq? '- (car expression)) (- (M_value(cadr expression) '()) (M_value (caddr expression) '() )))
      ((eq? '/ (car expression)) (quotient (M_value(cadr expression) '()) (M_value (caddr expression) '() )))
      ((eq? '% (car expression)) (modulo (M_value(cadr expression) '()) (M_value (caddr expression) '() )))
      )))

(define M_boolean
  (lambda (expression state)
    (cond
      ((null? expression) (error 'parser "invalid op"))
      ((eq? '== (car expression)) (eq? (M_value (cadr expression) '()) (M_value (caddr expression) '())))
      ((eq? '!= (car expression)) (not (eq? (M_value (cadr expression) '()) (M_value (caddr expression) '()))))
      ((eq? '< (car expression)) (< (M_value (cadr expression) '()) (M_value (caddr expression) '())))
      ((eq? '> (car expression)) (> (M_value (cadr expression) '()) (M_value (caddr expression) '())))
      ((eq? '<= (car expression)) (<= (M_value (cadr expression) '()) (M_value (caddr expression) '())))
      ((eq? '>= (car expression)) (>= (M_value (cadr expression) '()) (M_value (caddr expression) '())))
      ((eq? '&& (car expression)) (and (M_boolean (cadr expression) '()) (M_boolean (caddr expression) '())))
      ((eq? '|| (car expression)) (or (M_boolean (cadr expression) '()) (M_boolean (caddr expression) '())))
      ((eq? '! (car expression)) (not (M_boolean (cadr expression) '())))
      )))

(define M_declare
  (lambda (var val state)
    (cond
      [(null? state) (M_declare var val '(()()))]
      [(null? val) (rebuild (cons var (car state)) (cons 'undf (cadr state)))]
      [else (rebuild (cons var (car state)) (cons val (cadr state)))]
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
  


(define return
  (lambda (statement)
    (cond
    ((null? statement) #f)
    ((and (eq? (car statement) 'return) (eq? (length statement) 2)) #t)
    (else #f)
    )))

(define declaration
  (lambda (statement)
    (cond
      ((null? statement) #f)
      ((and (eq? (car statement) 'var) (eq? (length statement) 2)) #t)
      (else #f)
      )))

(define declare-and-assign
  (lambda (statement)
    (cond
      ((null? statement) #f)
      ((and (eq? (car statement) 'var) (eq? (length statement) 3)) #t)
      (else #f)
      )))

(define assignment
  (lambda (statement)
    (cond
      ((null? statement) #f)
      ((and (eq? (car statement) '=) (eq? (length statement) 2)) #t)
      (else #f)
      )))


(define rebuild
  (lambda (lis1 lis2)
    (cons  lis1 (list lis2))
    ))