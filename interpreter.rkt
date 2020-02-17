#lang racket
(require "simpleParser.rkt")
(require "statefunction.rkt")


; Implement  +,-,*,/,% as well as ==, !=,<,>,<=,>=, and &&,||,!.
; Variables may be true, false or an integer


;We need to find a way to return a number if the expression is simply a number

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



(define M_retrieve
  (lambda (input state)
    (cond
      ((null? state) null)
      ((null? input) null)
      ((eq? input (caar state)) (caadr state))
      (else (M_retrieve input (rebuild (cdr (car state)) (cdr (cadr state)))))
      )))



(define M_declare
  (lambda (var val state)
    (cond
      [(null? state) (M_declare var val '(()()))]
      [(null? val) (rebuild (cons var (car state)) (cons 'undf (cadr state)))]
      [else (rebuild (cons var (car state)) (cons val (cadr state)))]
      )))



(define rebuild
  (lambda (lis1 lis2)
    (cons  lis1 (list lis2))
    ))