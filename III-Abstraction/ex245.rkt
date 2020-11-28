;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex245) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; An NFunction (numerical function) is a function
; that takes a Number as its sole argument and returns
; a single Number.

; FUNCTION DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; NFunction NFunction -> Boolean
; Return #true if f1 and f2 return the same values
; for the arguments 1.2, 3 and -5.775, #false otherwise.
(define (function=at-1.2-3-and-5.775? f1 f2)
  (and (equal? (f1 1.2) (f2 1.2))
       (equal? (f1 3) (f2 3))
       (equal? (f1 -5.775) (f2 -5.775))))

(define (dummy-1 a)
  a)

(define (dummy-2 a)
  23)

(check-expect (function=at-1.2-3-and-5.775? dummy-1 dummy-2)
              #false)
(check-expect (function=at-1.2-3-and-5.775? dummy-1 dummy-1)
              #true)

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Can we hope to define function=?, which determines whether two functions from
; numbers to numbers are equal? If so, define the function. If not, explain why
; and consider the implication that you have encountered the first easily
; definable idea for which you cannot define a function.

; The function would have to deal with an infinite list of arguments that it
; would have to pass to the two functions and then compare the return values,
; all wrapped in a logical "and". There is no way to deal with an infinite
; list, as it would take infinitely long, not mentioning disk space.