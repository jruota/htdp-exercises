;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex118) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;1) (define (f x) x)
;
;2) (define (f x) y)
;
;3) (define (f x y) 3)
;
; According to the BSL Grammar, a legal def-expression has the following shape:
;
;     (define (variable variable variable ...) expr)
;
; All of the above adhere to that. The differences are as follows:
;
; 1) The variable "x" is the only expression in the function body.
;
; 2) The variable "x" is not used in the function body. The function body
;    uses another variable, variable "y".
;
; 3) This function definition has to arguments, the variables "x" and "y",
;    but does not use any in its body. Its body consists solely of the value 3.