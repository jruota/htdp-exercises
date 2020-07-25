;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex119) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;1) (define (f "x") x)
;
; Syntactically illegal because the variable "f" is followed by the value "x"
; and not another variable.
;
;2) (define (f x y z) (x))
;
; Syntactically illegal because "(x)" is not a valid expression
; (it is a variable enclosed in parentheses).