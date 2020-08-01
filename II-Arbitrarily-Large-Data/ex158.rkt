;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex158) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A List-of-shots is one of: 
; – '()
; – (cons Shot List-of-shots)
; interpretation the collection of shots fired

; A Shot is a Number.
; interpretation represents the shot's y-coordinate

; A ShotWorld is List-of-numbers. 
; interpretation each number on such a list
;   represents the y-coordinate of a shot

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define HEIGHT 80) ; distances in terms of pixels 
(define WIDTH 100)
(define XSHOTS (/ WIDTH 2))
 
; graphical constants 
(define BACKGROUND (empty-scene WIDTH HEIGHT))
(define SHOT (triangle 3 "solid" "red"))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; ------------------------------------------------------------------------------

; Explain what main does.

; main is being passed the initial world state and calls big-bang with this
; world state. big-bang installs all the handlers and translates the world
; state into an image.
; Here main is just a shorter function to type than big-bang.

; ------------------------------------------------------------------------------

; ShotWorld -> ShotWorld 
(define (main w0)
  (big-bang w0
    [on-tick tock]
    [on-key keyh]
    [to-draw to-image]))
 
; ShotWorld -> ShotWorld 
; moves each shot up by one pixel 
(define (tock w)
  (cond
    [(empty? w) '()]
    [else (remove-shots (cons (sub1 (first w)) (tock (rest w))))]))

(check-expect (tock '())
              '())
(check-expect (tock (cons 10 (cons 100 (cons 0 '()))))
              (cons 9 (cons 99 (cons -1 '()))))
 
; ShotWorld KeyEvent -> ShotWorld 
; adds a shot to the world if the space bar is hit 
(define (keyh w ke)
  (if (key=? ke " ") (cons HEIGHT w) w))

(check-expect (keyh (cons 10 '()) "a")
              (cons 10 '()))
(check-expect (keyh (cons 10 '()) " ")
              (cons HEIGHT (cons 10 '())))
 
; ShotWorld -> Image 
; adds each shot y on w at (XSHOTS,y} to BACKGROUND
(define (to-image w)
  (cond
    [(empty? w) BACKGROUND]
    [else (place-image SHOT XSHOTS (first w)
                       (to-image (rest w)))]))

(check-expect (to-image '())
              BACKGROUND)
(check-expect (to-image (cons 10 (cons 100 (cons 0 '()))))
              (place-image
               SHOT
               XSHOTS
               10
               (place-image
                SHOT
                XSHOTS
                100
                (place-image
                 SHOT
                 XSHOTS
                 0
                 BACKGROUND))))

; ShotWorld -> ShotWorld
; Remove all shots that are
; outside the canvas, i.e. they
; are no longer visible.
(define (remove-shots w)
  (cond
    [(empty? w) '()]
    [(cons? w)
     (if (<= (first w) (* -1/2 (image-height SHOT)))
         (remove-shots (rest w))
         (cons (first w)
               (remove-shots (rest w))))]))

(check-expect (remove-shots '())
              '())
(check-expect (remove-shots (cons 10
                                  (cons -100
                                        (cons (* -1/2 (image-height SHOT))
                                              (cons 0
                                                    '())))))
              (cons 10 (cons 0 '())))
                           