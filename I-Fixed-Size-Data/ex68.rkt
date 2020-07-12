;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex68) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct ball [location velocity])
; A Ball is a Structure:
;     (make-ball Posn Velocity)
; Interpretation:
;     The two-dimensional coordinates
;     and the x- and y-velocities of a ball.

(define-struct vel [deltax deltay])
; A Velocity is a Structure:
;     (make-vel Number Number)
; The x- and y-velocities of an object.

(define ball1
  (make-ball (make-posn 30 40) (make-vel -10 5)))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define-struct ballf [x y deltax deltay])
; A BallF is a Structure:
;     (make-ballf Number Number Number Number)
; Interpretation:
;     The x- and y-coordinates as well as
;     the respective velocities of a ball.
;     It is a flat representation of the
;     Ball structure.

(define ball2
  (make-ballf 30 40 -10 5))