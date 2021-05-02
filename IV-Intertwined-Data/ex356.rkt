;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex356) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A BSL-fun-expr is one of: 
; – Number
; – Symbol
; – FuncAppl
; – (make-add BSL-fun-expr BSL-var-expr)
; – (make-mul BSL-fun-expr BSL-var-expr)

(define-struct func-appl [name arg])
; A FuncAppl (short for function application) is a structure:
;     (make-func-appl Symbol BSL-fun-expr)
; Interpretation:
;     The name and the argument of a function.
;     Represents a function application.

(define-struct add [left right])
; An Add is a structure:
;     (make-add BSL-fun-expr BSL-fun-expr)
; Interpretation:
;     Represents an addition and
;     its two operands.

(define-struct mul [left right])
; A Mul is a structure:
;     (make-mul BSL-fun-expr BSL-fun-expr)
; Interpretation:
;     Represents a multiplication and
;     its two operands.

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; (k (+ 1 1))
(make-func-appl 'k (make-add 1 1))

; (* 5 (k (+ 1 1)))
(make-mul 5 (make-func-appl 'k (make-add 1 1)))

; (* (i 5) (k (+ 1 1)))
(make-mul (make-func-appl 'i 5) (make-func-appl 'k (make-add 1 1)))
