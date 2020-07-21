;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex96) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A UFO is a Posn. 
; interpretation (make-posn x y) is the UFO's location 
; (using the top-down, left-to-right convention)
 
(define-struct tank [loc vel])
; A Tank is a structure:
;   (make-tank Number Number). 
; interpretation (make-tank x dx) specifies the position:
; (x, HEIGHT) and the tank's speed: dx pixels/tick 
 
; A Missile is a Posn. 
; interpretation (make-posn x y) is the missile's place

(define-struct aim [ufo tank])
(define-struct fired [ufo tank missile])

; A SIGS (space invade game state) is one of: 
; – (make-aim UFO Tank)
; – (make-fired UFO Tank Missile)
; interpretation represents the complete state of a 
; space invader game

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; colors can be found at
; https://docs.racket-lang.org/draw/color-database___.html

; graphical constants
(define WIDTH 200)    ; -> ENTRY POINT
(define HEIGHT WIDTH)

(define GROUND (rectangle WIDTH (* 2/50 WIDTH) "solid" "SeaGreen"))

(define BACKGROUND (overlay/align
                    "middle"
                    "bottom"
                    GROUND
                    (empty-scene WIDTH HEIGHT "DarkSlateGray")))

(define TANK (rectangle (* 1/6 WIDTH) (* 2/50 WIDTH) "solid" "olive"))
(define TANK-Y (- HEIGHT (+ (* 1/2 (image-height TANK))
                            (image-height GROUND))))
(define TANK-HEIGHT (image-height TANK))
(define MISSILE (triangle (* 3/100 WIDTH) "solid" "red"))
(define UFO (overlay (circle (* 1/25 WIDTH) "solid" "green")
                     (rectangle (* 1/6 WIDTH) (* 1/50 WIDTH) "solid" "green")))

; physical constants
(define UFO-SPEED 3)
(define MISSILE-SPEED (* 2 UFO-SPEED))
(define TANK-SPEED UFO-SPEED)

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(make-aim (make-posn 20 10) (make-tank 28 -3))

(place-image
 UFO
 20
 10
 (place-image
  TANK
  28
  TANK-Y
  BACKGROUND))

(make-fired (make-posn 20 10)
            (make-tank 28 -3)
            (make-posn 28 (- HEIGHT TANK-HEIGHT)))

(place-image
 UFO
 20
 10
 (place-image
  MISSILE
  28
  (- HEIGHT TANK-HEIGHT)
  (place-image
   TANK
   28
   TANK-Y
   BACKGROUND)))

(make-fired (make-posn 20 100)
            (make-tank 100 3)
            (make-posn 22 103))

; change the order of MISSILE and UFO so that the MISSILE is visible
(place-image
 MISSILE
 22
 103
 (place-image
  UFO
  20
  100
  (place-image
   TANK
   100
   TANK-Y
   BACKGROUND)))
