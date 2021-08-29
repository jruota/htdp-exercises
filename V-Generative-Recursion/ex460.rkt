;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex460) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define ε 0.1)
(define δ .3)

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; [Number -> Number] Number Number -> Number
; Approximate the area under the graph of f by dividing
; the interval [a, b] into sufficiently small intervals
; and then using the Kepler method.
; Assumption: (<= a b)
(define (integrate-dc f a b)
  (cond
    [(<= (- b a) δ)
     (integrate-kepler f a b)]
    [else
     (local ((define mid (/ (+ a b) 2)))
       ; – IN –
       (+ (integrate-dc f a mid) (integrate-dc f mid b)))]))
 
(check-within (integrate-dc (lambda (x) 20) 12 22) 200 ε)
(check-within (integrate-dc (lambda (x) (* 2 x)) 0 10) 100 ε)
(check-within (integrate-dc (lambda (x) (* 3 (sqr x))) 0 10) 1000 ε)

; from ex458.rkt ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; [Number -> Number] Number Number -> Number
; Compute the area under the graph of f between a and b.
; Assumption: (< a b) holds.
(define (integrate-kepler f a b)
  (local ((define mid (/ (+ a b) 2))

          ; Number Number -> Number
          ; Calculate the are of the trapezoid
          ; between points x and y.
          ; Assume: (<= x y).
          (define (trapezoid-area x y)
            (* 1/2 (- y x) (+ (f y) (f x)))))
    ; – IN –
;    (+ (* 1/2 (- mid a) (+ (f mid) (f a)))
;       (* 1/2 (- b mid) (+ (f b) (f mid))))))
    (+ (trapezoid-area a mid)
       (trapezoid-area mid b))))
