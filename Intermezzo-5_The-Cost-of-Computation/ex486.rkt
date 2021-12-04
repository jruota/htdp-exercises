;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex486) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; N -> N
; Map numbers to numbers
; according to the definition
; of the function.
(define (f n)
  (+ (* n n) n))

; N -> N
; Map numbers to numbers
; according to the definition
; of the function.
(define (g n)
  (* n n))

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;          1   10    100     1000      10000

;   f(n)   2  110  10100  1001000  100010000

;   g(n)   1  100  10000  1000000  100000000

; 2*g(n)   2  200  20000  2000000  200000000

; Therefore f(n) <= 2*g(n) for all n >= 0.
; That is c = 2 and bigEnough = 0.
