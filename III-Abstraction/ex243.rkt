;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex243) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define (f x) x)

; 1.
; A list containing the function value f.
(cons f '())

; 2.
; Function application returning the function value f.
(f f)

; 3.
; A list containing the function value f, the number ten and the number
; ten again (return value of the function application (f 10)). 
(cons f (cons 10 (cons (f 10) '())))