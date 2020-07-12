;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex62) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define LOCKED "locked")
(define CLOSED "closed")
(define OPEN "open")

; A DoorState is one of:
; – LOCKED
; – CLOSED
; – OPEN

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; DoorState -> DoorState
; simulates a door with an automatic door closer
(define (door-simulation initial-state)
  (big-bang initial-state
    [on-tick door-closer 3]
    [on-key door-action]
    [to-draw door-render]))

; DoorState -> DoorState
; closes an open door over the period of one tick 
(define (door-closer state-of-door)
  (cond
    [(string=? LOCKED state-of-door) LOCKED]
    [(string=? CLOSED state-of-door) CLOSED]
    [(string=? OPEN state-of-door) CLOSED]))

(check-expect (door-closer LOCKED)
              LOCKED)
(check-expect (door-closer CLOSED)
              CLOSED)
(check-expect (door-closer OPEN)
              CLOSED)

; ... consider why our table suffices.
; While there are four state transitions in the graphic,
; only three can be achieved through an key event. The
; fourth entry in the table shows that one cannot achieve
; a state transition from "open" to "closed" by pressing a
; key. There the table is complete.

; DoorState KeyEvent -> DoorState
; turns key event k into an action on state s 
(define (door-action s k)
  (cond
    [(and (string=? LOCKED s) (string=? "u" k))
     CLOSED]
    [(and (string=? CLOSED s) (string=? "l" k))
     LOCKED]
    [(and (string=? CLOSED s) (string=? " " k))
     OPEN]
    [else s]))

(check-expect (door-action LOCKED "u") CLOSED)
(check-expect (door-action CLOSED "l") LOCKED)
(check-expect (door-action CLOSED " ") OPEN)
(check-expect (door-action OPEN "a") OPEN)
(check-expect (door-action CLOSED "a") CLOSED)

; DoorState -> Image
; translates the state s into a large text image
(define (door-render s)
  (text s 40 "red"))

(check-expect (door-render CLOSED)
              (text CLOSED 40 "red"))