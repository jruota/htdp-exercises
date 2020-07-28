;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex138) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;         |---------------------
;        \|/                   |
; A List-of-amounts is one of: |
; – '()                        |
; – (cons PositiveNumber List-of-amounts)
; Interpretation:
;     A list of amounts of money.

; List-of-amounts -> Number
; Compute the sum of the amounts
; in loa.
(define (sum loa)
  (cond
    [(empty? loa) 0]
    [(cons? loa)
     (+ (first loa)
        (sum (rest loa)))]))

(check-expect (sum '())
              0)
(check-expect (sum (cons 1 '()))
              1)
(check-expect (sum (cons 1 (cons 2 '())))
              3)
(check-expect (sum (cons 1 (cons 2 (cons 3 '()))))
              6)
(check-expect (sum (cons 1 (cons 2 (cons 3 (cons 444 '())))))
              450)