;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex284) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; This function application simply returns its argument.

((lambda (x) x) (lambda (x) x))

; ––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––

; This function application results in another function application that is
; equal to the one above, which in turn returns its argument. The final result
; is equal to the one above.

((lambda (x) (x x)) (lambda (x) x))

; ––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––

; Endless loop of a function applying itself to itself.
; ((lambda (x) (x x)) (lambda (x) (x x)))