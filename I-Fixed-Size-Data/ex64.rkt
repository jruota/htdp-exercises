;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex64) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Does it matter which strategy you follow?

; No it does not matter which strategy one follows, since
; the random version does not make steps in the wrong direction.

; Posn -> Number
; Measure the Manhattan distance of the given posn to the origin.
(define (manhattan-distance p)
  (+ (abs (posn-x p))
     (abs (posn-y p))))

(check-expect (manhattan-distance (make-posn 0 0))
              0)
(check-expect (manhattan-distance (make-posn 1 0))
              1)
(check-expect (manhattan-distance (make-posn 0 1))
              1)
(check-expect (manhattan-distance (make-posn 2 3))
              5)
(check-expect (manhattan-distance (make-posn -2 3))
              5)
(check-expect (manhattan-distance (make-posn 2 -3))
              5)
(check-expect (manhattan-distance (make-posn -2 -3))
              5)