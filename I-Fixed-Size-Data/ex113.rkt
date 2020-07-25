;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex113) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define-struct aim [ufo tank])
(define-struct fired [ufo tank missile])

; A UFO is a Posn. 
; interpretation (make-posn x y) is the UFO's location 
; (using the top-down, left-to-right convention)
 
(define-struct tank [loc vel])
; A Tank is a structure:
;   (make-tank Number Number). 
; interpretation (make-tank x dx) specifies the position:
; (x, HEIGHT) and the tank's speed: dx pixels/tick 
 
; A Missile is a Posn. 
; interpretation (make-posn x y) is the missile's place

; A SIGS is one of: 
; – (make-aim UFO Tank)
; – (make-fired UFO Tank Missile)
; interpretation represents the complete state of a 
; space invader game

; Any -> Boolean
; Is s an element of the SIGS collection.
(define (sigs? s)
  (or (aim? s) (fired? s)))

(check-expect (sigs? (make-aim (make-posn 10 10)
                               (make-tank 30 3)))
              #true)
(check-expect (sigs? (make-fired (make-posn 23 14)
                                 (make-tank 25 3)
                                 (make-posn 2 5)))
              #true)

(check-expect (sigs? empty-image)
              #false)
(check-expect (sigs? (make-posn 20 2))
              #false)
(check-expect (sigs? "hello")
              #false)
(check-expect (sigs? 55)
              #false)
(check-expect (sigs? #true)
              #false)

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A Coordinate is one of: 
; – a NegativeNumber 
; interpretation on the y axis, distance from top
; – a PositiveNumber 
; interpretation on the x axis, distance from left
; – a Posn
; interpretation an ordinary Cartesian point

; Any -> Boolean
; Is c an element of the Coordinate collection.
(define (coordinate? c)
  (or (number? c)
      (posn? c)))

(check-expect (coordinate? 0)
              #true)
(check-expect (coordinate? 34)
              #true)
(check-expect (coordinate? -4)
              #true)
(check-expect (coordinate? (make-posn 34 1))
              #true)

(check-expect (coordinate? "hello")
              #false)
(check-expect (coordinate? empty-image)
              #false)
(check-expect (coordinate? #false)
              #false)

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define-struct vcat [x hap])
; A VCat is a structure:
;     (make-vcat Number Number)
; where the following condition applies:
;     – hap is a number greater or equal to 0.
; Interpretation:
;     The cat's current x-coordinate
;     and its happiness level.

(define RED "red")
(define GREEN "green")
(define BLUE "blue")
; A Color is any of:
;     – RED
;     – GREEN
;     – BLUE
; Interpretation:
;     The color of the chameleon.

(define-struct vcham [x hap col])
; A VCham is a structure:
;     (make-vcham Number Number Color)
; where the following condition applies:
;     – hap is a number greater or equal to 0.
; Interpretation:
;     The chameleon's current x-coordinate,
;     its happiness level and its color.

; A VAnimal is either
; – a VCat
; – a VCham

; Any -> Boolean
; Is a an element of the VAnimal collection.
(define (vanimal? a)
  (or (vcat? a)
      (vcham? a)))

(check-expect (vanimal? (make-vcat 100 100))
              #true)
(check-expect (vanimal? (make-vcham 23 12 GREEN))
              #true)

(check-expect (vanimal? 12)
              #false)
(check-expect (vanimal? "hello")
              #false)
(check-expect (vanimal? empty-image)
              #false)
(check-expect (vanimal? #true)
              #false)
(check-expect (vanimal? (make-posn 98 324))
              #false)