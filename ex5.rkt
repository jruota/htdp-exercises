;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex5) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; BACKGROUND ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Entry point for scaling
(define WIDTH 800)

(define HEIGHT (* 3/5 WIDTH))

(define SCENE (empty-scene WIDTH HEIGHT "Sky Blue"))

; TREE ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; Colors taken from https://docs.racket-lang.org/draw/color-database___.html

(define CROWN (circle (* 1/6 WIDTH) "solid" "Dark Green"))
(define TRUNK (rectangle (* 1/4 (image-width CROWN))
                         (* 4/5 (image-height CROWN))
                         "solid" "brown"))
(define TREE (overlay/offset CROWN
                             0
                             (* 4/5 (image-height CROWN))
                             TRUNK))

; SUN ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define SUN (radial-star (round (* 0.033 WIDTH))    ; point count
                         (* 1/12 WIDTH)             ; inner radius
                         (* 1/8 WIDTH)              ; outer radius
                         "solid"
                         "yellow"))

; CLOUD ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define CLOUD (overlay/offset
               (ellipse (* 1/4 WIDTH) (* 1/6 HEIGHT) "solid" "white")
               (* 1/15 (* 1/4 WIDTH))
               (* -1/3 (* 1/6 HEIGHT))
               (circle (* 1/13 WIDTH) "solid" "white")))

; SUN + CLOUD ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define SUN+CLOUD (overlay/offset
                   CLOUD
                   (* -1/4 (image-width SUN))
                   (* -1/4 (image-height SUN))
                   SUN))

; GRASS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define GRASS (rectangle WIDTH (/ WIDTH 15) "solid" "LawnGreen"))

; COMBINING PARTS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define IMAGE
  (place-image/align
   GRASS
   0
   (- HEIGHT (image-height GRASS))
   "left"
   "top"
   (place-image/align
    TREE
    (* 3/4 WIDTH)
    HEIGHT
    "middle"
    "bottom"
    (place-image/align
     SUN+CLOUD
     (* 1/4 WIDTH)
     (* 1/4 HEIGHT)
     "middle"
     "middle"
     SCENE))))

IMAGE