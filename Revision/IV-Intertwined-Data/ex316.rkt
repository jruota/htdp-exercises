;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex316) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; An Atom is one of: 
; – Number
; – String
; – Symbol

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Any -> Boolean
; Is a an Atom?
(define (atom? a)
  (or (number? a)
      (string? a)
      (symbol? a)))

(check-expect (atom? 23) #true)
(check-expect (atom? "twenty-three") #true)
(check-expect (atom? 'symbol) #true)
(check-expect (atom? #true) #false)
(check-expect (atom? empty-image) #false)
(check-expect (atom? (make-posn 1 2)) #false)
(check-expect (atom? atom?) #false)
