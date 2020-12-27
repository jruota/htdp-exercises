;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex279) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; First note that when running the program with only the function definitions,
; the second end fifth definition will throw errors, while all other definitions
; will not. This is because the function body is only interpreted when there is
; an function application.

; ––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––

(lambda (x y) (x y y))
;((lambda (x y) (x y y) 1 2))

; The function application throws the following error:
;     lambda: expected only one expression for the function body,
;             but found 2 extra parts
; The body of any function definition needs to be an expression, here there
; are three expressions, which is illegal syntax.

; ––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––

; (lambda () 10)

; Following error is thrown:
;     lambda: expected (lambda (variable more-variable ...) expression),
;             but found no variables
; Self-explanatory.
; It is an illegal expression because it is not syntactically correct.

; ––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––

(lambda (x) x)
((lambda (x) x) 2)

; This is a legal expression since it correctly applies
; the syntax of "lambda" expressions.

; ––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––

(lambda (x y) x)
((lambda (x y) x) 1 2)

; This is a legal expression since it correctly applies the syntax of "lambda"
; expressions. It simply ignores the second argument.

; ––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––

; (lambda x 10)

; Following error is thrown:
;     lambda: expected (lambda (variable more-variable ...) expression),
;             but found something else
; There are no parentheses around the single parameter "x", which is
; syntactically incorrect.