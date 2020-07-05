;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex42) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; PHYSICAL CONSTANTS 
(define WIDTH-OF-WORLD 200)

; ENTRY POINT
(define WHEEL-RADIUS 5)
(define WHEEL-DISTANCE (* WHEEL-RADIUS 3))

; GRAPHICAL CONSTANTS
(define WHEEL
  (circle WHEEL-RADIUS "solid" "black"))

(define SPACE
  (rectangle WHEEL-DISTANCE 0 "solid" "white"))

(define BOTH-WHEELS
  (beside WHEEL SPACE WHEEL))

(define CHASSIS
  (rectangle (+ WHEEL-DISTANCE (* 6 WHEEL-RADIUS))
             (* 2 WHEEL-RADIUS)
             "solid" "red"))

(define CABIN
  (rectangle (+ (image-width SPACE) (* 2 WHEEL-RADIUS))
             (* 1 WHEEL-RADIUS)
             "solid" "red"))

(define CAR (overlay/align/offset
             "middle"
             "bottom"
             BOTH-WHEELS
             0
             (* -1 WHEEL-RADIUS)
             (above CABIN CHASSIS)))

(define BACKGROUND (empty-scene WIDTH-OF-WORLD
                                (+ (image-height CAR) (* 2 WHEEL-RADIUS))))

(define Y-CAR (- (image-height BACKGROUND) (* 1/2 (image-height CAR))))

; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A WorldState is a Number.
; interpretation the number of pixels between
; the left border of the scene and
; the right edge of the car

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; WorldState -> WorldState
; launches the program from some initial state 
(define (main ws)
   (big-bang ws
     [on-tick tick-handler]
     [to-draw render]))

; WorldState -> Image
; places the car into the BACKGROUND scene,
; where the given world state denotes the right
; edge of the car
 (define (render cw)
   (place-image CAR (+ cw (* 1/2 (image-width CAR))) Y-CAR BACKGROUND))

(check-expect (render (* 1/2 (image-width BACKGROUND)))
              (place-image CAR
                           (+ (* 1/2 (image-width BACKGROUND))
                              (* 1/2 (image-width CAR)))
                           Y-CAR
                           BACKGROUND))
 
; WorldState -> WorldState
; Moves the car by 3 pixels for every clock tick.
; Examples:
;    given: 20, expect: 23
;    given: 78, expect: 81
(define (tick-handler ws)
  (+ ws 3))

(check-expect (tick-handler 20) 23)
(check-expect (tick-handler 78) 81)