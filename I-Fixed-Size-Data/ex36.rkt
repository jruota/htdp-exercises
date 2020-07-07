;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex36) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; Image -> Number
; Count the number of pixels in img.
(define (image-area img)
  (* (image-width img) (image-height img)))

(image-area (rectangle 100 20 "outline" "orange"))
(image-area (square 50 "outline" "orange"))
(image-area (circle 25 "outline" "orange"))
(image-area (circle 50 "outline" "orange"))