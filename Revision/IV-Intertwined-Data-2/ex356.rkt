;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex356) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

(define-struct func-appl [name expr])
; A FuncAppl (function application) is a structure:
;     (make-func-appl Symbol BSL-fun-expr)
; Interpretation:
;     Represents the application of a function. The "name"
;     field is the name of the function and the "expr"
;     field is the function argument.

; A BSL-fun-expr is one of: 
; – Number
; – Symbol 
; – (make-add BSL-fun-expr BSL-fun-expr)
; – (make-mul BSL-fun-expr BSL-fun-expr)
; – (make-func-appl BSL-fun-expr BSL-fun-expr)

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; 1. (k (+ 1 1))
(make-func-appl 'k (make-add 1 1))

; 2. (* 5 (k (+ 1 1)))
(make-mul 5 (make-func-appl 'k (make-add 1 1)))

; 3. (* (i 5) (k (+ 1 1)))
(make-mul (make-func-appl 'i 5) (make-func-appl 'k (make-add 1 1)))
