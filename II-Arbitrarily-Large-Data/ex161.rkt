;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex161) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A List-of-numbers is one of: 
; – '()
; – (cons Number List-of-numbers)

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define HOURLY-RATE 14)

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; List-of-numbers -> List-of-numbers
; computes the weekly wages for all given weekly hours
(define (wage* whrs)
  (cond
    [(empty? whrs) '()]
    [else (cons (wage (first whrs)) (wage* (rest whrs)))]))

(check-expect (wage* '())
              '())
(check-expect (wage* (cons 28 '()))
              (cons (wage 28) '()))
(check-expect (wage* (cons 4 (cons 2 '())))
              (cons (wage 4) (cons (wage 2) '())))
 
; Number -> Number
; computes the wage for h hours of work
(define (wage h)
  (* HOURLY-RATE h))

(check-expect (wage 0)
              (* 0 HOURLY-RATE))
(check-expect (wage 4)
              (* 4 HOURLY-RATE))