;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex414) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; An N is one of:
; – 0
; – (add1 N)
; Interpretation:
;     The natural numbers including 0.

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define INTEGER-ERROR "expects integer greater or equal 0")

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; N -> Number
; Add up #i1/185 n times.
(define (add n)
  (local (; N -> Number
          ; Perform the actual addition.
          (define (main n0)
            (cond
              [(zero? n0) 0]
              [else
               (+ #i1/185 (main (sub1 n0)))])))
    ; – IN –
    (if (and (integer? n) (>= n 0))
        (main n)
        (error INTEGER-ERROR))))

(check-expect (add 0) 0)
(check-within (add 1) 1/185 0.0001)
(check-error (add -3) INTEGER-ERROR)
(check-error (add 3.14) INTEGER-ERROR)

; Number -> N
; Count how often 1/185 can be subtracted
; from num until it is 0.
(define (sub num)
  (cond
    [(<= num 0) 0]
    [else (+ 1 (sub (- num 1/185)))]))

(check-expect (sub 0) 0)
(check-expect (sub 1/185) 1)

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(add 185)
(* (add 185) 1000000000)
(sub 1)
(sub #i1.0)