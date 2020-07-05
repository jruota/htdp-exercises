;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex43) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

; CONSTANTS FOR THE MOVEMENT ACCORDING TO A SINE WAVE
(define SPEED 3)

; move middle of the car to the edge of the canvas
(define AMPLITUDE (* 1/2 WIDTH-OF-WORLD))

; move edges of the car to the edges of the canvas,
; so that half of the car can still be seen
;(define AMPLITUDE (- (* 1/2 WIDTH-OF-WORLD) (* 1/2 (image-width CAR))))

; move edges of the car to the edges of the canvas,
; so that the car dissappears from the canvas
;(define AMPLITUDE (+ (* 1/2 WIDTH-OF-WORLD) (* 1/2 (image-width CAR))))

; makes the car start in the middle of the canvas
(define Y-SHIFT (* 1/2 WIDTH-OF-WORLD))
(define PHASE-SHIFT 0)
; the period has been defined such that the car takes
; the same time to travel the same distance as it would
; when moving at a constant speed;
; the multiplication by 2 of (/ WIDTH-OF-WORLD SPEED)
; has to be made, because the car covers the distance
; of two WIDTH-OF-WORLDs in one period
(define PERIOD (/ (* 2 pi) (* 2 (/ WIDTH-OF-WORLD SPEED))))


; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; An AnimationState is a Number.
; interpretation the number of clock ticks 
; since the animation started

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; WorldState -> WorldState
; launches the program from some initial state 
(define (main ws)
   (big-bang ws
     [on-tick tick-handler]
     [to-draw render]))

; WorldState -> Image
; places the car into the BACKGROUND scene,
; according to the given world state
(define (render cw)
  (place-image CAR
               (x-car cw)
               Y-CAR
               BACKGROUND))

(check-expect (render 0)
              (place-image CAR
                           (* 1/2 WIDTH-OF-WORLD)
                           Y-CAR
                           BACKGROUND))

; WorldState -> Number
; Return the x-position of the car.
; Move the car according to a sine wave.
(define (x-car ws)
  (+ (* AMPLITUDE
        (sin (+ (* PERIOD ws)
                PHASE-SHIFT)))
     Y-SHIFT))

(check-within (x-car 0)
              (* 1/2 WIDTH-OF-WORLD)
              0.1)
(check-within (x-car (* (* 2 (/ WIDTH-OF-WORLD SPEED)) 1/4))
              (+ (* 1/2 WIDTH-OF-WORLD) AMPLITUDE)
              0.1)
(check-within (x-car (* (* 2 (/ WIDTH-OF-WORLD SPEED)) 2/4))
              (* 1/2 WIDTH-OF-WORLD)
              0.1)
(check-within (x-car (* (* 2 (/ WIDTH-OF-WORLD SPEED)) 3/4))
              (- (* 1/2 WIDTH-OF-WORLD) AMPLITUDE)
              0.1)
(check-within (x-car (* (* 2 (/ WIDTH-OF-WORLD SPEED)) 4/4))
              (* 1/2 WIDTH-OF-WORLD)
              0.1)

; WorldState -> WorldState
; Moves the car according to a sine wave.
(define (tick-handler ws)
  (+ ws 1))

(check-expect (tick-handler 20) 21)
(check-expect (tick-handler 78) 79)