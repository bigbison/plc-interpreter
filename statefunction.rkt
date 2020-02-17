#lang racket
;*******************************************************
; EECS 345 Interpreter Part 1
;
; Ben Trabold
; Rembrandt van der Ploeg
;
; This file defines all of the helper functions used to manage the state
; Our state is a list of two lists, the first list being
; all variables and the second all of their values. ordered accordingly
; Example '((x y z) (1 4 7)) means x == 1, y == 4; z == 7.
;
;*******************************************************

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


