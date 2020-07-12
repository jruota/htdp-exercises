;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex61) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define RED 0)
(define GREEN 1)
(define YELLOW 2)

;(define RED "red")
;(define GREEN "green")
;(define YELLOW "yellow")
 
; An S-TrafficLight is one of:
; – RED
; – GREEN
; – YELLOW

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Which of the two is properly designed using the recipe for itemization?
; Which of the two continues to work if you change the constants
; to the following:

;    (define RED "red")
;    (define GREEN "green")
;    (define YELLOW "yellow")

; This version properly uses the design recipe for itemizations.
; It is only this version that will work when one changes the constants,
; since the previous version relies that the TrafficLight is represented
; by integers 0, 1 and 2.

; S-TrafficLight -> S-TrafficLight
; yields the next state, given current state cs
	
(define (tl-next-symbolic cs)
  (cond
    [(equal? cs RED) GREEN]
    [(equal? cs GREEN) YELLOW]
    [(equal? cs YELLOW) RED]))	

(check-expect (tl-next-symbolic RED) GREEN)
(check-expect (tl-next-symbolic YELLOW) RED)
(check-expect (tl-next-symbolic GREEN) YELLOW)