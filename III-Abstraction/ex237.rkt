;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex237) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Number Number -> Boolean
; Is the area of a square with side x larger than c?
(define (squared>? x c)
  (> (* x x) c))

(check-expect (squared>? 2 4)
              #false)
(check-expect (squared>? 2 3)
              #true)

(squared>? 3 10)
(squared>? 4 10)
(squared>? 5 10)