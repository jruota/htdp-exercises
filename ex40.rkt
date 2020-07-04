;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex40) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; WorldState -> WorldState
; Moves the car by 3 pixels for every clock tick.
; Examples:
;    given: 20, expect: 23
;    given: 78, expect: 81
(define (tick-handler ws)
  (+ ws 3))

(check-expect (tick-handler 20) 22)
(check-expect (tick-handler 78) 82)