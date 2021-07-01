;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex418) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; An N is one of:
; – 0
; – (add1 N)
; Interpretation:
;     The natural numbers.

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define POWER-ERROR "expects an integer greater or equal 1 as power")

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Number N -> Number
; Raise x to the power of y.
(define (my-expt x y)
  (local (; N -> Number
          ; Perform the actual calculation.
          (define (main n)
            (cond
              [(zero? n) 1]
              [else (* x (main (sub1 n)))])))
    ; – IN –
    (if (and (integer? y) (>= y 0))
        (main y)
        (error POWER-ERROR))))

(check-expect (my-expt 3 0)
              1)
(check-expect (my-expt 3 3)
              27)
(check-error (my-expt 4 3.14)
             POWER-ERROR)
(check-error (my-expt 4 -1)
             POWER-ERROR)

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define inex (+ 1 #i1e-12))
(define exac (+ 1 1e-12))

(my-expt inex 30)
(my-expt exac 30)

; The inexact result is more useful, as it clearly signals that the result is
; inexact.
; The exact result has an unknown number of decimal places, from which it is
; hard to estimate the error (1.0000000000300000000004350...).