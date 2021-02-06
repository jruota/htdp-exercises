;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex302) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define x (cons 1 x))

; The binding occurrence of x is the x right after the "define". But since this
; is a constant definition, the right hand side needs to be evaluated first
; (it should be a list), but it uses a reference to x that has not been defined.