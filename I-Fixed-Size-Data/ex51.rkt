;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex51) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A TrafficLight is one of the following Strings:
; – "red"
; – "green"
; – "yellow"
; interpretation the three strings represent the three 
; possible states that a traffic light may assume

(define-struct ttl [light time])
; A TTL (timed TrafficLight) is a structure:
;     (make-ttl TrafficLight PositiveNumber)
; Interpretation:
;     Combines the current state of the traffick light
;     and the time left for the main program to run.

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; ONLY ENTRY-POINT
(define WIDTH 250)
(define HEIGHT WIDTH)

(define BACKGROUND (empty-scene WIDTH HEIGHT "black"))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Number -> Image
; Start the program and run it for x seconds.
(define (main x)
  (big-bang (make-ttl "red" x)
    [on-draw render]
    [on-tick tock 1]    ; clock ticks once per second
    [stop-when times-up]))

; TTL -> Image
; Render the current state of the world.
(define (render ttl)
  (place-image (circle (* 1/2 4/5 WIDTH) "solid" (ttl-light ttl))
               (* 1/2 WIDTH)
               (* 1/2 HEIGHT)
               BACKGROUND))

(check-expect (render (make-ttl "red" 100))
              (place-image (circle (* 1/2 4/5 WIDTH) "solid" "red")
                           (* 1/2 WIDTH)
                           (* 1/2 HEIGHT)
                           BACKGROUND))
(check-expect (render (make-ttl "yellow" 82))
              (place-image (circle (* 1/2 4/5 WIDTH) "solid" "yellow")
                           (* 1/2 WIDTH)
                           (* 1/2 HEIGHT)
                           BACKGROUND))
(check-expect (render (make-ttl "green" 1))
              (place-image (circle (* 1/2 4/5 WIDTH) "solid" "green")
                           (* 1/2 WIDTH)
                           (* 1/2 HEIGHT)
                           BACKGROUND))

; TTL -> TTL
; Change the state of the world by changing
; the color of the traffick light according to
; traffic-light-next and subtracting from the
; time left to run the program.
(define (tock ttl)
  (make-ttl (traffic-light-next (ttl-light ttl))
            (sub1 (ttl-time ttl))))

(check-expect (tock (make-ttl "red" 23))
              (make-ttl "green" 22))

; TTL -> Boolean
; Return #true if the time for the program is up.
(define (times-up ttl)
  (<= (ttl-time ttl) 0))

(check-expect (times-up (make-ttl "red" 1))
              #false)
(check-expect (times-up (make-ttl "yellow" 0))
              #true)
(check-expect (times-up (make-ttl "green" -1))
              #true)

; TrafficLight -> TrafficLight
; yields the next state given current state s
(define (traffic-light-next s)
  (cond
    [(string=? "red" s) "green"]
    [(string=? "green" s) "yellow"]
    [(string=? "yellow" s) "red"]))

(check-expect (traffic-light-next "red")
              "green")
(check-expect (traffic-light-next "green")
              "yellow")
(check-expect (traffic-light-next "yellow")
              "red")