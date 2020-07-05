;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex39) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; PHYSICAL CONSTANTS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define WIDTH-OF-WORLD 200)

; ENTRY POINT
(define WHEEL-RADIUS 5)

(define WHEEL-DISTANCE (* WHEEL-RADIUS 3))

; GRAPHICAL CONSTANTS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
                                (+ (* 2 WHEEL-RADIUS) (image-height CAR))))

(define Y-CAR (- (image-height BACKGROUND) (* 1/2 (image-height CAR))))

(place-image CAR
             (* 1/2 WIDTH-OF-WORLD)
             Y-CAR
             BACKGROUND)