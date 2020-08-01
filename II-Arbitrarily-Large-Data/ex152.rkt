;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex152) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; An N is one of: 
; – 0
; – (add1 N)
; interpretation represents the counting numbers

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define DOT (circle 5 "solid" "orange"))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; N Image -> Image
; Produce a vertical arrangement of
; n copies of img.
(define (column n img)
  (cond
    [(zero? n) empty-image]
    [(positive? n)
     (overlay/xy img
                 0
                 (image-height img)
                 (column (sub1 n) img))]))

(check-expect (column 0 DOT)
              empty-image)
(check-expect (column 1 DOT)
              DOT)
(check-expect (column 2 DOT)
              (above DOT DOT))
(check-expect (column 5 DOT)
              (above DOT DOT DOT DOT DOT))

; N Image -> Image
; Produce a horizontal arrangement of
; n copies of img.
(define (row n img)
  (cond
    [(zero? n) empty-image]
    [(positive? n)
     (overlay/xy img
                 (image-width img)
                 0
                 (row (sub1 n) img))]))

(check-expect (row 0 DOT)
              empty-image)
(check-expect (row 1 DOT)
              DOT)
(check-expect (row 2 DOT)
              (beside DOT DOT))
(check-expect (row 5 DOT)
              (beside DOT DOT DOT DOT DOT))