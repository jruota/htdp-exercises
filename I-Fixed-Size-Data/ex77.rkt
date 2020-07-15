;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex77) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct pitsm [h m s])
; A PITSM is a structure:
;    (make-pitsm Number Number Number)
; The following restrictions apply:
;     – first number 0 <= n <= 23
;     – second number 0 <= n <= 59
;     – third number 0 <= n <= 59
; Interpretation:
;     Hours, minutes and seconds since midnight.