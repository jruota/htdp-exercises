;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex60) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; An N-TrafficLight (numeric TrafficLight) is one of:
; – 0 interpretation the traffic light shows red
; – 1 interpretation the traffic light shows green
; – 2 interpretation the traffic light shows yellow

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
    [on-tick tl-next-numeric 1]))

; Does the tl-next function convey its intention more clearly than the
; tl-next-numeric function? If so, why? If not, why not?

; The tl-next function conveys its intention more clearly than the
; tl-next-numeric function, because it spells its functionality
; out with a cond expression, which is closer to natural English.
; tl-next-numeric on the other hand uses more of a mathematical
; language. Also using Strings to represent TrafficLight makes
; it easier to read and understand.

; N-TrafficLight -> N-TrafficLight
; yields the next state, given current state cs
(define (tl-next-numeric cs)
  (modulo (+ cs 1) 3))

(check-expect (tl-next-numeric 0)
              1)
(check-expect (tl-next-numeric 2)
              0)
(check-expect (tl-next-numeric 1)
              2)
 
; TrafficLight -> Image
; renders the current state cs as an image
(define (tl-render current-state)
  (place-image
   (make-lightbulb (if (= current-state 0)
                       "solid"
                       "outline")
                   "red")
   (* 1/6 WIDTH)
   (* 1/2 HEIGHT)
   (place-image
    (make-lightbulb (if (= current-state 2)
                        "solid"
                        "outline")
                    "yellow")
    (* 3/6 WIDTH)
    (* 1/2 HEIGHT)
    (place-image
     (make-lightbulb (if (= current-state 1)
                         "solid"
                         "outline")
                     "green")
     (* 5/6 WIDTH)
     (* 1/2 HEIGHT)
     BACKGROUND))))

(check-expect (tl-render 0)
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

(check-expect (tl-render 2)
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

(check-expect (tl-render 1)
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