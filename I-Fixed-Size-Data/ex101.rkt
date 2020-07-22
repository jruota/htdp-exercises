;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex101) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

(define-struct sigs [ufo tank missile])
; A SIGS.v2 (short for SIGS version 2) is a structure:
;   (make-sigs UFO Tank MissileOrNot)
; interpretation represents the complete state of a
; space invader game
 
; A MissileOrNot is one of: 
; – #false
; – Posn
; interpretation#false means the missile is in the tank;
; Posn says the missile is at that location

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
(define UFO-RADIUS (* 1/25 WIDTH))
(define UFO-WING-HEIGHT (* 1/50 WIDTH))
(define UFO-WING-SPAN (* 1/6 WIDTH))
(define UFO (overlay (circle UFO-RADIUS "solid" "green")
                     (rectangle UFO-WING-SPAN UFO-WING-HEIGHT "solid" "green")))

(define TEXT-SIZE (* 1/10 WIDTH))

; physical constants
(define UFO-SPEED 3)
(define UFO-JUMP 13)    ; make this a positive, uneven number
(define MISSILE-SPEED (* 2 UFO-SPEED))
(define TANK-SPEED UFO-SPEED)

; for testing
(define SCENE (place-image UFO
                           (* 1/2 WIDTH)
                           (* 2 (image-height UFO))
                           (place-image TANK
                                        32
                                        TANK-Y
                                        BACKGROUND)))
; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; MissileOrNot Image -> Image 
; adds an image of missile m to scene s 
(define (missile-render.v2 m s)
  s)

(check-expect (missile-render.v2 #false SCENE)
              SCENE)
(check-expect (missile-render.v2 (make-posn 32 (- HEIGHT TANK-HEIGHT 10))
                                 SCENE)
              (place-image MISSILE
                           32
                           (- HEIGHT TANK-HEIGHT 10)
                           SCENE))