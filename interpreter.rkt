#lang racket

; Implement  +,-,*,/,% as well as ==, !=,<,>,<=,>=, and &&,||,!.
; Variables may be true, false or an integer



(define M_value
  (lambda (expression state)
    (cond
      ((null? expression) (error 'parser "parser should have caught this"))
      ((number? expression) expression)
      ((eq? '+ (car (cdr expression))) (+ (M_value(car expression) '()) (M_value (car (cdr (cdr expression))) '() )))
      ((eq? '* (car (cdr expression))) (* (M_value(car expression) '()) (M_value (car (cdr (cdr expression))) '() )))
      ((eq? '- (car (cdr expression))) (- (M_value(car expression) '()) (M_value (car (cdr (cdr expression))) '() )))
      ((eq? '/ (car (cdr expression))) (quotient (M_value(car expression) '()) (M_value (car (cdr (cdr expression))) '() )))
      ((eq? '% (car (cdr expression))) (modulo (M_value(car expression) '()) (M_value (car (cdr (cdr expression))) '() )))
      )))
