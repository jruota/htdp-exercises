;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex437) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [List-of X] -> [List-of X]
; A general, structurally recursive function.
(define (special P)
  (cond
    [(empty? P) (solve P)]
    [else
     (combine-solutions
       P
       (special (rest P)))]))

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; special computes the length of its input

; Any -> 0
; Return zero.
(define (solve x)
  0)

; Any Number -> Number
; Add 1 to y, discard x.
(define (combine-solutions x y)
  (+ 1 y))

(check-expect (special (range 1 11 1)) 10)

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; special negates each number on the given list of numbers

;; Any -> '()
;; Return the empty list.
;(define (solve x)
;  '())
;
;; [List-of Number] [List-of Number] -> [List-of Number]
;; Negate the first number in x (if it is not negative
;; already) and add it to y.
;(define (combine-solutions x y)
;  (cons (if (> (first x) 0)
;            (* -1 (first x))
;            (first x))
;        y))
;
;(check-expect (special (range 5 -6 -1)) (list -5 -4 -3 -2 -1 0 -1 -2 -3 -4 -5))

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; special uppercases the given list of strings

;; Any -> '()
;; Return the empty list.
;(define (solve x)
;  '())
;
;; [List-of String] [List-of String] -> [List-of String]
;; Add the uppercase version of the first item
;; in x to the front of y.
;(define (combine-solutions x y)
;  (cons (if (string=? "" (first x))
;            ""
;            (string-upcase (first x)))
;        y))
;
;(check-expect (special (list "hello" "" "world" "banana" "orange" "alabama"))
;              (list "HELLO" "" "WORLD" "BANANA" "ORANGE" "ALABAMA"))
