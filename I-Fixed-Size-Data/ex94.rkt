;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex94) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; colors can be found at
; https://docs.racket-lang.org/draw/color-database___.html

; graphical constants
(define WIDTH 400)    ; -> ENTRY POINT
(define HEIGHT (* 3/2 WIDTH))

(define GROUND (rectangle WIDTH (* 2/50 WIDTH) "solid" "SeaGreen"))

(define BACKGROUND (overlay/align
                    "middle"
                    "bottom"
                    GROUND
                    (empty-scene WIDTH HEIGHT "DarkSlateGray")))

(define TANK (rectangle (* 1/6 WIDTH) (* 2/50 WIDTH) "solid" "olive"))
(define MISSILE (triangle (* 3/100 WIDTH) "solid" "red"))
(define UFO (overlay (circle (* 1/25 WIDTH) "solid" "green")
                     (rectangle (* 1/6 WIDTH) (* 1/50 WIDTH) "solid" "green")))

; physical constants
(define TANK-Y (- HEIGHT (+ (* 1/2 (image-height TANK))
                            (image-height GROUND))))

(define UFO-SPEED 3)
(define MISSILE-SPEED (* 2 UFO-SPEED))
(define TANK-SPEED UFO-SPEED)

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define SCENE1
  (place-image
   UFO
   (* 2/3 WIDTH)
   (* 1/3 HEIGHT)
   (place-image
    MISSILE
    (* 2/5 WIDTH)
    (* 3/5 HEIGHT)
    (place-image
     TANK
     (* 2/5 WIDTH)
     TANK-Y
     BACKGROUND))))

SCENE1