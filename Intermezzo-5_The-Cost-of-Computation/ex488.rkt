;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex488) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; NOTE ---------------------------------------------------------------------
; The natural logarithm is used here. For an alternative base, use (log n b)
; where b is the base.
; END NOTE -----------------------------------------------------------------

; N -> N
; Map numbers to numbers
; according to the definition
; of the function.
(define (f n)
  (* n (log n)))

; N -> N
; Map numbers to numbers
; according to the definition
; of the function.
(define (g n)
  (* n n))

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; NOTE -------------------------------------
; The results for f(n) are rounded up to the
; third digit.
; END NOTE ---------------------------------

;                                   f(n)                                    g(n)

;     1                            0.000                                       1

;    10                           23.026                                     100

;   100                          460.517                                   10000

;  1000                         6907.755                                 1000000

; 10000                        92103.404                               100000000

; Therefore f(n) <= g(n) for all n >= 1.
; That is c = 1 and bigEnough = 1.
; f belongs to O(g).
