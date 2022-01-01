;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex511) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; With all functions, use the "Check Syntax" button to show the binding and
; bound occurences.

;(λ (x) x)
;((λ (x) x) 5)

; y is undefined
;(λ (x) y)
;((λ (x) y) 5)

; x is free
;(λ (y) (λ (x) y))
;(((λ (y) (λ (x) y)) 5) 6)

; identity function applied to itself and then to 5
;((λ (x) x) (λ (x) x))
;(((λ (x) x) (λ (x) x)) 5)

; inifite loop(s)
;((λ (x) (x x)) (λ (x) (x x)))
;(((λ (x) (x x)) (λ (x) (x x))) 5)

; the first function application applies (λ (y) (λ (x) y)) to (λ (z) z)
; and returns (λ (x) y), that returns (λ (z) z) whatever argument it is given;
; this is then applied to (λ (w) w) and returns (λ (z) z)
(((λ (y) (λ (x) y)) (λ (z) z)) (λ (w) w))
((((λ (y) (λ (x) y)) (λ (z) z)) (λ (w) w)) 5)

(define (func1 y)
  (local ((define (func2 x)
            y))
    ; – IN –
    func2))

(define (func3 z)
  z)

(define (func4 w)
  w)

((func1 func3) func4)
(((func1 func3) func4) 5)
