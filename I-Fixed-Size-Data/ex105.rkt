;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex105) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A Coordinate is one of: 
; – a NegativeNumber 
; interpretation on the y axis, distance from top
; – a PositiveNumber 
; interpretation on the x axis, distance from left
; – a Posn
; interpretation an ordinary Cartesian point

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define WIDTH 300)
(define HEIGHT 300)

(define BACKGROUND (empty-scene WIDTH HEIGHT))
(define DOT (circle 3 "solid" "red"))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; JUST TO GENERATE RANDOM COORDINATES
; Number -> Number
; Produce a random number in the range [-x/2, x/2).
(define (random-number x)
  (- (random x) (floor (/ x 2))))

; Coordinate -> Image
; Draw the c on a WIDTH by HEIGHT canvas.
; If (abs c) is greater than 500, it
; will be outside the canvas.
(define (draw-coordinate c)
  (cond
    [(posn? c)
     (place-image DOT (posn-x c) (posn-y c) BACKGROUND)]
    [(negative? c)
     (place-image DOT (* 1/2 (image-width BACKGROUND)) (abs c) BACKGROUND)]
    [(positive? c)
     (place-image DOT c (* 1/2 (image-height BACKGROUND)) BACKGROUND)]))

(check-expect (draw-coordinate (make-posn 211 183))
              (place-image DOT 211 183 BACKGROUND))
(check-expect (draw-coordinate -215)
              (place-image DOT (* 1/2 (image-width BACKGROUND)) 215 BACKGROUND))
(check-expect (draw-coordinate 112)
              (place-image DOT
                           112
                           (* 1/2 (image-height BACKGROUND))
                           BACKGROUND))

; EXAMPLES ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(draw-coordinate -191)
(draw-coordinate -109)

(draw-coordinate 230)
(draw-coordinate 53)

(draw-coordinate (make-posn 118 108))
(draw-coordinate (make-posn 234 47))
