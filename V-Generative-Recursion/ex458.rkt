;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex458) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define ε 0.1)

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; [Number -> Number] Number Number -> Number
; Compute the area under the graph of f between a and b.
; Assumption: (< a b) holds.
(define (integrate-kepler f a b)
  (local ((define mid (/ (+ a b) 2)))
    ; – IN –
    (+ (* 1/2 (- mid a) (+ (f mid) (f a)))
       (* 1/2 (- b mid) (+ (f b) (f mid))))))
 
(check-within (integrate-kepler (lambda (x) 20) 12 22) 200 ε)
(check-within (integrate-kepler (lambda (x) (* 2 x)) 0 10) 100 ε)
; this test is off by 375, as two trapezoids are a very imprecise approximation
; of a polynomial
(check-within (integrate-kepler (lambda (x) (* 3 (sqr x))) 0 10) 1000 ε)