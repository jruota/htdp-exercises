;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex420) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require plot)

; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; An N is one of:
; – 0
; – (add1 N)
; Interpretation:
;     The natural numbers.

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define EMPTY-ERROR "expected non-empty list")

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; N -> [List-of Number]
; Return the list (-0.99 -0.99^2 -0.99^3 ... -0.99^n).
(define (oscillate n)
  (local ((define (O i)
            (cond
              [(> i n) '()]
              [else
               (cons (expt #i-0.99 i) (O (+ i 1)))])))
    (O 1)))

; N -> Number
; Return the x-th element of oscillate x.
(define (xth-oscillate x)
  (list-ref (oscillate x) (sub1 x)))

(check-within (xth-oscillate 1) #i-0.99 (expt 10 -16))
(check-within (xth-oscillate 15)  #i-0.8600583546412884 (expt 10 -16))

; from exercise 419 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; [List-of Number] -> Number
; Calculate the sum of the numbers in lon.
(define (sum lon)
  (cond
    [(empty? lon) 0]
    [else (+ (first lon) (sum (rest lon)))]))

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(oscillate 15)

(sum (oscillate #i1000.0))
(sum (reverse (oscillate #i1000.0)))

(- (* 1e+16 (sum (oscillate #i1000.0)))
   (* 1e+16 (sum (reverse (oscillate #i1000.0)))))

; (plot (function xth-oscillate 0 100 #:label "oscillate on [0, 100)"))
