;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex115) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require racket/format)

; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A TrafficLight is one of the following Strings:
; – "red"
; – "green"
; – "yellow"
; interpretation the three strings represent the three 
; possible states that a traffic light may assume 


(define MESSAGE
  "TrafficLight expected, given some other value")

; Any Any -> Boolean
; are the two values elements of TrafficLight and, 
; if so, are they equal

(define (light=? a-value another-value)
  (cond
    [(not (light? a-value))
     (error (format "TrafficLight expected as 1st argument, ~a given" a-value))]
    [(not (light? another-value))
     (error (format "TrafficLight expected as 2nd argument, ~a given"
                    another-value))]
    [else (string=? a-value another-value)]))

(check-expect (light=? "red" "red") #true)
(check-expect (light=? "red" "green") #false)
(check-expect (light=? "green" "green") #true)
(check-expect (light=? "yellow" "yellow") #true)

(check-error (light=? "red" 23)
             "TrafficLight expected as 2nd argument, 23 given")
(check-error (light=? #true "red")
             "TrafficLight expected as 1st argument, #t given")

; Any -> Boolean
; is the given value an element of TrafficLight
(define (light? x)
  (cond
    [(string? x) (or (string=? "red" x)
                     (string=? "green" x)
                     (string=? "yellow" x))]
    [else #false]))

(check-expect (light? "red")
              #true)
(check-expect (light? "green")
              #true)
(check-expect (light? "yellow")
              #true)

(check-expect (light? "hello")
              #false)
(check-expect (light? 23)
              #false)
(check-expect (light? empty-image)
              #false)
(check-expect (light? #true)
              #false)
(check-expect (light? (make-posn 77 88))
              #false)