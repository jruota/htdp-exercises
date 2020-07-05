;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex44) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; PHYSICAL CONSTANTS 
(define WIDTH-OF-WORLD 1200)

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

(define tree
  (underlay/xy (circle (* 2 WHEEL-RADIUS) "solid" "green")
               (* 9/5 WHEEL-RADIUS) (* 3 WHEEL-RADIUS)
               (rectangle (* 2/5 WHEEL-RADIUS) (* 4 WHEEL-RADIUS)
                          "solid" "brown")))

(define HEIGHT (+ (max (image-height CAR)
                       (image-height tree))
                  (* 2 WHEEL-RADIUS)))

(define BACKGROUND (place-image tree
                                (* 1/2 WIDTH-OF-WORLD)
                                (- HEIGHT (* 1/2 (image-height tree)))
                                (empty-scene WIDTH-OF-WORLD HEIGHT)))

(define Y-CAR (- (image-height BACKGROUND) (* 1/2 (image-height CAR))))

; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A WorldState is a Number.
; interpretation the number of pixels between
; the left border of the scene and the car

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; WorldState -> WorldState
; launches the program from some initial state 
(define (main ws)
   (big-bang ws
     [on-tick tick-handler]
     [on-mouse hyper]
     [to-draw render]))

; WorldState -> Image
; places the car into the BACKGROUND scene,
; according to the given world state 
 (define (render cw)
   (place-image CAR cw Y-CAR BACKGROUND))

(check-expect (render (* 1/2 (image-width BACKGROUND)))
              (place-image CAR
                           (* 1/2 (image-width BACKGROUND))
                           Y-CAR
                           BACKGROUND))
 
; WorldState -> WorldState
; Moves the car by 3 pixels for every clock tick.
(define (tick-handler ws)
  (+ ws 3))

(check-expect (tick-handler 20) 23)
(check-expect (tick-handler 78) 81)

; WorldState Number Number String -> WorldState
; places the car at x-mouse
; if the given me is "button-down" 
; given: 21 10 20 "enter"
; wanted: 21
; given: 42 10 20 "button-down"
; wanted: 10
; given: 42 10 20 "move"
; wanted: 42
(define (hyper x-position-of-car x-mouse y-mouse me)
  (if (string=? me "button-down")
      x-mouse
      x-position-of-car))

(check-expect (hyper 21 10 20 "enter")
              21)
(check-expect (hyper 42 10 20 "button-down")
              10)
(check-expect (hyper 42 10 20 "move")
              42)