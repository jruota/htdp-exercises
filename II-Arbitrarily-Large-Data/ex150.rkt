;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex150) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; An N is one of: 
; – 0
; – (add1 N)
; interpretation represents the counting numbers

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; N -> Number
; computes (+ n pi) without using +
(define (add-to-pi n)
  (cond
    [(zero? n) pi]
    [(positive? n)
     (add1 (add-to-pi (sub1 n)))]))

(check-within (add-to-pi 3) (+ 3 pi) 0.001)

; Why does the skeleton use check-within?

; pi is an inexact number and adding a natural number to an inexact number
; results in an inexact number. Functions returning inexact number
; have to be tested with check-withing to allow for this inexactness.

; N -> Number
; Compute (+ n x) without using +.
(define (add n x)
  (cond
    [(zero? n) x]
    [(positive? n)
     (add1 (add (sub1 n) x))]))

(check-expect (add 0 1.4)
              1.4)
(check-expect (add 3 1.4)
              4.4)