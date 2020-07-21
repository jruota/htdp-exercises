;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex97) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; SIGS -> Image
; renders the given game state on top of BACKGROUND 
; for examples see figure 32
(define (si-render s)
  (cond
    [(aim? s)
     (ufo-render (aim-ufo s)
                  (tank-render (aim-tank s) BACKGROUND))]
    [(fired? s)
     (missile-render
       (fired-missile s)
       (ufo-render (fired-ufo s)
                   (tank-render (fired-tank s)
                                BACKGROUND)))]))

(check-expect (si-render (make-aim (make-posn 20 10) (make-tank 28 -3)))
              (place-image
               UFO
               20
               10
               (place-image
                TANK
                28
                TANK-Y
                BACKGROUND)))

(check-expect (si-render (make-fired (make-posn 20 10)
                                     (make-tank 28 -3)
                                     (make-posn 28 (- HEIGHT TANK-HEIGHT))))
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
                 BACKGROUND))))

(check-expect (si-render (make-fired (make-posn 20 100)
                                     (make-tank 100 3)
                                     (make-posn 22 103)))
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
                 BACKGROUND))))

; Tank Image -> Image 
; adds t to the given image im
(define (tank-render t im)
  (place-image TANK
               (tank-loc t)
               TANK-Y
               im))

(check-expect (tank-render (make-tank (* 1/2 WIDTH) TANK-SPEED) BACKGROUND)
              (place-image TANK
                           (* 1/2 WIDTH)
                           TANK-Y
                           BACKGROUND))
 
; UFO Image -> Image 
; adds u to the given image im
(define (ufo-render u im)
  (place-image UFO
               (posn-x u)
               (posn-y u)
               im))

(check-expect (ufo-render (make-posn (* 1/2 WIDTH) (* 1/2 WIDTH)) BACKGROUND)
              (place-image UFO
                           (* 1/2 WIDTH)
                           (* 1/2 WIDTH)
                           BACKGROUND))

; Missile Image -> Image 
; adds m to the given image im
(define (missile-render m im)
  (place-image MISSILE
               (posn-x m)
               (posn-y m)
               im))

(check-expect (missile-render (make-posn (* 1/2 WIDTH)
                                         (* 1/2 WIDTH))
                                         BACKGROUND)
              (place-image MISSILE
                           (* 1/2 WIDTH)
                           (* 1/2 WIDTH)
                           BACKGROUND))

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;Compare this expression:
;
;    (tank-render
;      (fired-tank s)
;      (ufo-render (fired-ufo s)
;                  (missile-render (fired-missile s)
;                                  BACKGROUND)))
;
;with this one:
;
;    (ufo-render
;      (fired-ufo s)
;      (tank-render (fired-tank s)
;                   (missile-render (fired-missile s)
;                                   BACKGROUND)))
;
;When do the two expressions produce the same result?

; The two expression produce the same result ONLY when the
; TANK, UFO and MISSILE do not occupy the same place, i.e.
; if they do not lie on top of another.
; If they did, the first expression would  show the TANK
; on top of the UFO and the MISSILE behind the UFO which
; might result in only being able to see the TANK.
; The second expression would flip the position of the
; TANK and UFO.