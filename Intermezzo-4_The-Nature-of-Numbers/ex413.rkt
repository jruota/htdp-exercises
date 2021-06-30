;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex413) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define-struct inex [mantissa sign exponent])
; An Inex is a structure: 
;   (make-inex N99 S N99)

; An S is one of:
; – 1
; – -1

; An N99 is an N between 0 and 99 (inclusive).

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define N99 99)
(define RANGE-ERROR "number is out of range")

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Inex Inex -> Inex
; Multiply in1 and in2.
; Signal an error if the result is out of range.
(define (inex* in1 in2)
  (local ((define mantissa-new (* (inex-mantissa in1) (inex-mantissa in2)))
          (define exponent-new (+ (* (inex-sign in1) (inex-exponent in1))
                                  (* (inex-sign in2) (inex-exponent in2))))
          (define res (format-mantissa
                       (make-inex mantissa-new
                                  (if (>= exponent-new 0) 1 -1)
                                  (abs exponent-new))
                       N99)))
    ; – IN –
    (if (> (inex-exponent res) 99)
        (error RANGE-ERROR)
        res)))

(check-expect (inex* (create-inex 3 1 3) (create-inex 5 1 5))
              (create-inex 15 1 8))
(check-expect (inex* (create-inex 3 1 1) (create-inex 34 1 7))
              (create-inex 10 1 9))
(check-expect (inex* (create-inex 12 1 2) (create-inex 3 -1 5))
              (create-inex 36 -1 3))

(check-error (inex* (create-inex 34 1 99) (create-inex 3 1 99))
             RANGE-ERROR)

; Inex N -> Inex
; Make sure that the mantissa of in
; is less than or equal to n by adapting
; the exponent of in.
(define (format-mantissa in n)
  (local (; Inex -> Inex
          ; Perform the actual change of mantissa
          ; and exponent.
          (define (inex-transform in0)
            (local ((define mantissa (inex-mantissa in0))
                    (define sign (inex-sign in0))
                    (define exponent (inex-exponent in0)))
              ; – IN –
              (if (> mantissa n)
                  (inex-transform
                   (make-inex (inexact->exact (round (/ mantissa 10)))
                              sign
                              (add1 exponent)))
                  (make-inex mantissa sign exponent)))))
    ; – IN –
    (inex-transform in)))

(check-expect (format-mantissa (make-inex 112 1 0) 99)
              (create-inex 11 1 1))
(check-expect (format-mantissa (make-inex 10000 -1 4) 99)
              (create-inex 10 -1 7))

; from figure 143 --------------------------------------------------------------

; N Number N -> Inex
; makes an instance of Inex after checking the arguments
(define (create-inex m s e)
  (cond
    [(and (<= 0 m 99) (<= 0 e 99) (or (= s 1) (= s -1)))
     (make-inex m s e)]
    [else (error "bad values given")]))