;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex67) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define SPEED 3)
(define-struct balld [location direction])
(make-balld 10 "up")

; Interpret this code fragment and create other instances of balld.

; Functions that deal with "balld"'s use the constant SPEED when
; they need the numerical value of the ball's speed. Its direction
; is not conveyed by a signed number, but by a String, presumably
; the Strings "up" and "down".

(make-balld 0 "down")
(make-balld 23 "up")