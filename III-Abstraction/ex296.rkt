;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex296) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; Origin is at (100, 100), the radius of 5 has been scaled up to 50.
; The y-axis is positive downward. 
(define BACKGROUND
  (scene+line
   (scene+line
    (empty-scene 200 200) 0 100 200 100 "black")
   100 0 100 200 "black"))

(define MIDPOINT (circle 3 "solid" "red"))
(define CIRCLE (circle 50 "solid" (make-color 255 0 0 128)))
(define DOT (circle 3 "solid" "black"))

(place-image
 DOT 100 100
 (place-image
  DOT 100 190
  (place-image
   DOT 90 130
   (place-image
    MIDPOINT 130 140
    (place-image
     CIRCLE 130 140 BACKGROUND)))))