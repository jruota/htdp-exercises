;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex455) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define ε 0.001)
(define DELTA 0.000001)

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; [Number -> Number] -> Number
; Return an approximation of the slope of f at r1;
(define (slope f r1)
  (* (/ 1 (* 2 ε)) (- (f (+ r1 ε)) (f (- r1 ε)))))

(check-within (slope (lambda (x) x) 50) 1 DELTA)
(check-within (slope (lambda (x) (* -1 x)) 50) -1 DELTA)
(check-within (slope (lambda (x) 7) -123.664) 0 DELTA)
(check-within (slope quadratic 0) 0 DELTA)
(check-within (slope quadratic 1) 2 DELTA)
(check-within (slope quadratic -2) -4 DELTA)

(define (quadratic x)
  (* x x))