;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex53) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; An LR (short for launching rocket) is one of:
; – "resting"
; – NonnegativeNumber
; interpretation "resting" represents a grounded rocket
; a number denotes the height of a rocket in flight

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define HEIGHT 300) ; distances in pixels 
(define WIDTH  100)
(define YDELTA 3)
 
(define BACKG  (empty-scene WIDTH HEIGHT))
(define ROCKET (rectangle 5 30 "solid" "red"))
 
(define CENTER (/ (image-height ROCKET) 2))

; SCENARIOS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; LR = 0
(place-image ROCKET
             (* 1/2 WIDTH)
             (- HEIGHT CENTER HEIGHT)
             BACKG)

; LR = HEIGHT
(place-image ROCKET
             (* 1/2 WIDTH)
             (- HEIGHT CENTER)
             BACKG)

; LR = "resting"
(place-image ROCKET
             (* 1/2 WIDTH)
             (- HEIGHT CENTER)
             BACKG)

; LR = (* 1/2 HEIGHT)
(place-image ROCKET
             (* 1/2 WIDTH)
             (- HEIGHT CENTER (* 1/2 HEIGHT))
             BACKG)