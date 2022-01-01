;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex513) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A Lam is one of: 
; – a Symbol
; – (make-λ-fun Symbol Lam)
; – (make-λ-app make-λ-fun Lam)

(define-struct λ-fun [para body])
; A λ-fun-def (short for lambda function definition) is a structure:
;     (make-λ-fun Symbol Lam)
; Interpretation:
;     The parameter and body of a function.
;     Represents a function definition.

(define-struct λ-app [fun arg])
; A λ-fun-app (short lambda function application) is a structure:
;     (make-λ-app λ-fun-def Lam)
; Interpretation:
;     The function definition and the argument of a function application.
;     Represents a function application.

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define ex1 (make-λ-fun 'x 'x))
(define ex2 (make-λ-fun 'x 'y))
(define ex3 (make-λ-fun 'y (make-λ-fun 'x 'y)))
