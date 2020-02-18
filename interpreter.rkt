#lang racket
(require "simpleParser.rkt")
(require "statefunction.rkt")


; Implement  +,-,*,/,% as well as ==, !=,<,>,<=,>=, and &&,||,!.
; Variables may be true, false or an integer

;if, if else, while, conditional

;We need to find a way to return a number if the expression is simply a number

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