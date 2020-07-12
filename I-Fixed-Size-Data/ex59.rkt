;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex59) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A TrafficLight is one of the following Strings:
; – "red"
; – "green"
; – "yellow"
; interpretation the three strings represent the three 
; possible states that a traffic light may assume

; A Mode is one of the following Strings:
; – "solid"
; – "outline"
; Interpretation:
;     The mode in which the lightbulbs are to
;     be drawn.

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; ONLY ENTRY-POINT
(define WIDTH 180)
(define HEIGHT (* 1/3 WIDTH))

(define BACKGROUND (empty-scene WIDTH HEIGHT "black"))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; TrafficLight -> TrafficLight
; simulates a clock-based American traffic light
(define (traffic-light-simulation initial-state)
  (big-bang initial-state
    [to-draw tl-render]
    [on-tick tl-next 1]))

; TrafficLight -> TrafficLight
; yields the next state, given current state cs
(define (tl-next cs)
  (cond
    [(string=? cs "red") "green"]
    [(string=? cs "yellow") "red"]
    [(string=? cs "green") "yellow"]))

(check-expect (tl-next "red")
              "green")
(check-expect (tl-next "yellow")
              "red")
(check-expect (tl-next "green")
              "yellow")
 
; TrafficLight -> Image
; renders the current state cs as an image
(define (tl-render current-state)
  (place-image
   (make-lightbulb (if (string=? current-state "red")
                       "solid"
                       "outline")
                   "red")
   (* 1/6 WIDTH)
   (* 1/2 HEIGHT)
   (place-image
    (make-lightbulb (if (string=? current-state "yellow")
                        "solid"
                        "outline")
                    "yellow")
    (* 3/6 WIDTH)
    (* 1/2 HEIGHT)
    (place-image
     (make-lightbulb (if (string=? current-state "green")
                         "solid"
                         "outline")
                     "green")
     (* 5/6 WIDTH)
     (* 1/2 HEIGHT)
     BACKGROUND))))

(check-expect (tl-render "red")
              (place-image
               (circle (* 5/36 WIDTH) "solid" "red")
               (* 1/6 WIDTH)
               (* 1/2 HEIGHT)
               (place-image
                (circle (* 5/36 WIDTH) "outline" "yellow")
                (* 3/6 WIDTH)
                (* 1/2 HEIGHT)
                (place-image
                 (circle (* 5/36 WIDTH) "outline" "green")
                 (* 5/6 WIDTH)
                 (* 1/2 HEIGHT)
                 BACKGROUND))))

(check-expect (tl-render "yellow")
              (place-image
               (circle (* 5/36 WIDTH) "outline" "red")
               (* 1/6 WIDTH)
               (* 1/2 HEIGHT)
               (place-image
                (circle (* 5/36 WIDTH) "solid" "yellow")
                (* 3/6 WIDTH)
                (* 1/2 HEIGHT)
                (place-image
                 (circle (* 5/36 WIDTH) "outline" "green")
                 (* 5/6 WIDTH)
                 (* 1/2 HEIGHT)
                 BACKGROUND))))

(check-expect (tl-render "green")
              (place-image
               (circle (* 5/36 WIDTH) "outline" "red")
               (* 1/6 WIDTH)
               (* 1/2 HEIGHT)
               (place-image
                (circle (* 5/36 WIDTH) "outline" "yellow")
                (* 3/6 WIDTH)
                (* 1/2 HEIGHT)
                (place-image
                 (circle (* 5/36 WIDTH) "solid" "green")
                 (* 5/6 WIDTH)
                 (* 1/2 HEIGHT)
                 BACKGROUND))))

; Mode TrafficLight -> Image
; Return the image of a single lightbulb
; in the given Mode and Color.
(define (make-lightbulb m c)
  (circle (* 5/36 WIDTH) m c))

(check-expect (make-lightbulb "outline" "yellow")
              (circle (* 5/36 WIDTH) "outline" "yellow"))