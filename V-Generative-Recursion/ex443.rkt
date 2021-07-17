;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex443) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Given the header material for gcd-structural, a naive use of the design recipe
; might use the following template or some variant: 

; N[>= 1] N[>= 1] -> N
; Find the greatest common divisor of n and m.
(define (gcd-structural n m)
  (cond
    [(and (= n 1) (= m 1)) ...]
    [(and (> n 1) (= m 1)) ...]
    [(and (= n 1) (> m 1)) ...]
    [else
     (... (gcd-structural (sub1 n) (sub1 m)) ...
      ... (gcd-structural (sub1 n) m) ...
      ... (gcd-structural n (sub1 m)) ...)]))

; Why is it impossible to find a divisor with this strategy?

; First conditional:
;     Both numbers are equal to 1, therefore the greatest common divisor (gcd)
;     must be 1, as well.

; Second conditional:
;     The first number is greater than 1 and the second is equal to 1 and since
;     the number 1 has only 1 as a divisor, the gcd must be 1.

; Third conditional:
;     The same argument as the second conditional with the order of the
;     arguments flipped.

; Fourth conditional:
;     Deals with all of those cases where neither of the arguments is equal to
;     by delegating a different problem to a recursive call.

; The problem with approach is that only the first three conditionals return
; answers and their answer is always 1. If one were to find a single example
; where the gcd is not equal to 1 (say for the numbers 24 and 18) then one would
; be able that it impossible to find the right solution with this approach.