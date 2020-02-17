#lang racket
(require "simpleParser.rkt")


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