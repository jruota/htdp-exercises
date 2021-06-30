;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex416) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; An N is one of:
; – 0
; – (add1 N)
; Interpretation:
;     The natural numbers including 0.

; A ONE is one of:
; – 1
; – -1

; A Limit is one of:
; – #i0.0
; – +inf.0

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define INPUT-ERROR "expects 1 or -1 as input")

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; ONE -> Integer
; If one is 1 determine the integer n such that
; (expt #i10.0 n) is an inexact number while
; (expt #i10. (+ n 1)) is approximated with +inf.0.
; If one is -1 determine the smallest integer n such that
; (expt #i10.0 n) is still an inexact ISL+ number
; and (expt #i10. (- n 1)) is approximated with 0.
(define (find-maxmin-n one)
  (local (; Integer Limit [N -> N] -> Integer
          ; Perform the actual calculation.
          (define (find-n i l f)
            (cond
              [(= (expt #i10.0 i) l) i]
              [else (find-n (f i) l f)])))
    ; – IN –
    (cond
      [(= 1 one) (sub1 (find-n 1 +inf.0 add1))]
      [(= -1 one) (add1 (find-n -1 #i0.0 sub1))]
      [else (error INPUT-ERROR)])))

; Any -> N
; Determine the smallest integer n such that
; (expt #i10.0 n) is still an inexact ISL+ number
; and (expt #i10. (- n 1)) is approximated with 0.
; The argument x is discarded.
(define (find-zero x)
  (local (; Integer -> Integer
          ; Perform the actual calculation.
          (define (find-n n)
            (cond
              [(= (expt #i10.0 n) #i0.0) (add1 n)]
              [else (find-n (sub1 n))])))
    ; – IN –
    (find-n -1)))

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

(find-zero "hello world")
(expt #i10.0 -323)
(expt #i10.0 -324)

(find-maxmin-n 1)
(find-maxmin-n -1)