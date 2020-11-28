;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex244) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; NOTE

; Function names have been slightly changed to avoid the following error:

;     f: this name was defined previously and cannot be re-defined

; END NOTE

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; 1.
; Given the function value x, (f x) returns the return value of the function
; application (x 10).
(define (f x) (x 10))

; 2.
; Given the function value x, (f x) returns the return value of the function
; application (x f), the argument f being a function value.
(define (w x) (x f))

; 3.
; Given the function value x, return the return value of the function
; application (x 'a y 'b).
(define (v x y) (x 'a y 'b))

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define (dummy-f1 a)
  a)

(define (dummy-f2 a b c)
  (list a b c))

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(f dummy-f1)
(w dummy-f1)
(v dummy-f2 23)