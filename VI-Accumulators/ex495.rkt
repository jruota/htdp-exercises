;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex495) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [List-of Number] -> Number
; Return the sum of all numbers in alon.
(define (sum.v1 alon)
  (cond
    [(empty? alon) 0]
    [else (+ (first alon) (sum.v1 (rest alon)))]))

(check-expect (sum.v1 '()) 0)
(check-expect (sum.v1 (list 1 2 3 4 5 6 7 8 9 10)) (* 5 (+ 10 1)))

; [List-of Number] -> Number
; Return the sum of all numbers in alon.
(define (sum.v2 alon0)
  (local (; [List-of Number] Number -> Number
          ; Compute the sum of the numbers on alon.
          ; Accumulator a is the sum of the numbers 
          ; that alon lacks from alon0.
          (define (sum/a alon a)
            (cond
              [(empty? alon) a]
              [else (sum/a (rest alon)
                           (+ (first alon) a))])))
    ; – IN –
    (sum/a alon0 0)))

(check-expect (sum.v2 '()) 0)
(check-expect (sum.v2 (list 1 2 3 4 5 6 7 8 9 10))
              (* (/ 10 2) (+ 10 1)))

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; (sum.v1 '(10 4))
; == (+ 10 (sum.v1 '(4)))
; == (+ 10 (+ 4 (sum.v1 '())))
; == (+ 10 (+ 4 (+ 0)))
; == (+ 10 (+ 4 0))
; == (+ 10 4)
; == 14

; ------------------------------------------------------------------------------

; (sum.v2 '(10 4))
; == (sum/a '(10 4) a0)
; == (sum/a '(4) (+ 10 a0))
; == (sum/a '() (+ 4 (+ 10 a0))
; == (+ 4 (+ 10 a0))
; == (+ 4 (+ 10 0))
; == (+ 4 10)
; == 14

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define size 5000000)
(define list-of-numbers (build-list size add1))

(time (sum.v1 list-of-numbers))
(time (sum.v2 list-of-numbers))
