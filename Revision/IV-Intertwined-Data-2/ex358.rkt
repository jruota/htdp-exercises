;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex358) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

(define-struct func-appl [name arg])
; A FuncAppl (function application) is a structure:
;     (make-func-appl Symbol BSL-fun-expr)
; Interpretation:
;     Represents the application of a one-parameter function.
;     The "name" field is the name of the function
;     and the "arg" field is the function argument.

(define-struct func-def [name para body])
; A FuncDef (function definition) is a structure:
;     (make-func-def Symbol Symbol BSL-fun-expr)
; Interpretation:
;     Represents the definition of a one-parameter function.
;     The "name" field is the name of the function,
;     the "para" field is the name of the function parameter
;     and the "body" field is the function body.

; A BSL-fun-expr is one of: 
; – Number
; – Symbol 
; – (make-add BSL-fun-expr BSL-fun-expr)
; – (make-mul BSL-fun-expr BSL-fun-expr)
; – (make-func-appl BSL-fun-expr BSL-fun-expr)

; A BSL-fun-def* is one of:
; – '()
; – (cons FuncDef BSL-fun-def*)
; Interpretation:
;     Represents a definitions area that consists
;     of a number of one-argument function definitions.

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; 1.
(define (f x) (+ 3 x))
(define F (make-func-def 'f 'x (make-add 3 'x)))

; 2.
(define (g y) (f (* 2 y)))
(define G (make-func-def 'g 'y (make-func-appl 'f (make-mul 2 'y))))

; 3.
(define (h v) (+ (f v) (g v)))
(define H
  (make-func-def 'h
                 'v
                 (make-add (make-func-appl 'f 'v) (make-func-appl 'g 'v))))

(define da-fgh (list F G H))

(define FUNC-NOT-FOUND "there is no such function definition")

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; BSL-fun-def* Symbol -> BSL-fun-def
; Retrieve the definition of function f in da,
; signal an error if there is none.
(define (lookup-def da f)
  (cond
    [(empty? da)
     (error FUNC-NOT-FOUND)]
    [else
     (local ((define FIRST (first da)))
       ; – IN –
       (if (symbol=? f (func-def-name FIRST))
           FIRST
           (lookup-def (rest da) f)))]))

(check-expect (lookup-def da-fgh 'g) G)
(check-error (lookup-def da-fgh 'function)
             FUNC-NOT-FOUND)
  