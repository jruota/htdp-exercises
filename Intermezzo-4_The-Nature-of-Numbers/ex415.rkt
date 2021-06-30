;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex415) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; An N is one of:
; – 0
; – (add1 N)
; Interpretation:
;     The natural numbers including 0.

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Any -> N
; Determine the integer n such that
; (expt #i10.0 n) is an inexact number
; while (expt #i10. (+ n 1)) is
; approximated with +inf.0
; The argument x is discarded.
(define (find-10-to-the-n x)
  (local (; N -> N
          ; Perform the actual calculation.
          (define (find-n n)
            (cond
              [(= (expt #i10.0 n) +inf.0) (sub1 n)]
              [else (find-n (add1 n))])))
    ; – IN –
    (find-n 1)))

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(find-10-to-the-n "hello world")
(expt #i10.0 308)
(expt #i10.0 309)