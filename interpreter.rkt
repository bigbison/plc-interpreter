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
      ((symbol? expression) (retrieve-var-state expression state))
      ((eq? '+ (car expression)) (+ (M_value(cadr expression) state) (M_value (caddr expression) state )))
      ((eq? '* (car expression)) (* (M_value(cadr expression) state) (M_value (caddr expression) state )))
      ((eq? '- (car expression)) (- (M_value(cadr expression) state) (M_value (caddr expression) state )))
      ((eq? '/ (car expression)) (quotient (M_value(cadr expression) state) (M_value (caddr expression) state )))
      ((eq? '% (car expression)) (modulo (M_value(cadr expression) state) (M_value (caddr expression) state )))
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
      ((eq? 'return (car stmt)) #t)
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

;list of variables
(define var-list car)

;list of values
(define val-list cadr)

; rebuilds the correct state format from seperate lists of states and values
(define rebuild
  (lambda (lis1 lis2)
    (cons  lis1 (list lis2))
    ))

; first part of the state, example for '((x y z) (1 2 3))
; it returns '((x) (1))
(define first-state
  (lambda (state)
    (list (caar state) (caadr state))))

; rest of the state besides the first, ex. for '((x y z) (1 2 3))
; it returns '((y z) (2 3))
(define later-state
  (lambda (state)
    (list (cdar state) (cdadr state))))

; attatches the first part of the state to the last, helps with recursing
; through the state list
(define attatch-state
  (lambda (first later)
    (list (cons (car first) (car later)) (cons (cadr first) (cadr later)))))


; retrives the current value of a varaible
(define retrieve-var-state
  (lambda (var state)
    (cond
      ((null? state) null)
      ((null? var) null)
      ((eq? var (caar state)) (caadr state))
      (else (retrieve-var-state var (rebuild (cdr (car state)) (cdr (cadr state)))))
      )))

; assigns a value to a variable
(define assign-var-state
  (lambda (var val state)
    (cond
      ((null? state) null)
      ((null? val) null)
      ((null? var) null)
      ((eq? (car (var-list state)) var) (list (var-list state)
                                              (cons val (cdr (val-list state)))))
      (else (attatch-state (first-state state)
                           (assign-var-state var val (later-state state)))))))
