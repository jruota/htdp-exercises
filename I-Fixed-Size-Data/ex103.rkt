;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex103) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; NOTE - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

; A different approach is making space a structure, so that each
; animal would spacify how much space it needs in any given
; direction in three dimensional space. In that case there would
; not be any hardcoded number (magical numbers) in fits?.

; END NOTE - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define-struct spider [legs space])
; A Spider is a structure:
;     (make-spider Number Number)
; Interpretation:
;     The remaining legs of the spider
;     and the amount of space needed
;     in case of transport in cubic meters.

(define-struct elephant [space])
; An Elephant is a structure:
;     (make-elephant Number)
; Interpretation:
;     The space needed to transport the elephant
;     in cubic meters.

(define-struct boa-constrictor [length girth])
; A BoaConstrictor is a structure:
;     (make-boa-constrictor Number Number)
; Interpretation:
;     The length and girth of the boa constrictor
;     in centimeters.

(define-struct armadillo [weight length space])
; An Armadillo is a Structure:
;     (make-armadillo Number Number Number)
; Interpretation:
;     The weight in kilograms and the length in centimeters
;     of the armadillo, as well as the space needed to transport
;     the armadillo im cubic meters.

; A ZooAnimal is one of:
;     – Spider
;     – Elephant
;     – BoaConstrictor
;     – Armadillo

(define-struct cage [width depth height])
; A Cage is a structure:
;     (make-cage Number Number Number)
; Interpretation:
;     The width, depth and height of a cage
;     in meters.

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; ZooAnimal -> ???
; Template for a function that consumes
; ZooAnimals.
(define (zoo-function za)
  (cond
    [(spider? za)
     (... (spider-legs za) ... (spider-space za) ...)]
    [(elephant? za)
     (... (elephant-space za) ...)]
    [(boa-constrictor? za)
     (... (boa-constrictor-length za) ... (boa-constrictor-girth za) ...)]
    [(armadillo? za)
     (... (armadillo-weight za) ... (armadillo-length za) ...
      ... (armadillo-space za) ...)]))

; ZooAnimal Cage -> Boolean
; Determine whether the cage's volume
; is big enough for the animal.
(define (fits? za c)
  (cond
    [(spider? za)
     (if (and (>= (cage-width c) (cage-depth c) (cage-height c) 0.10)
              (>= (cage-volume c)
                  (spider-space za)))
         #true
         #false)]
    [(elephant? za)
     (if (and (>= (cage-width c) 7)
              (>= (cage-depth c) 4.5)
              (>= (cage-height c) 4.5)
              (>= (cage-volume c)
                  (elephant-space za)))
         #true
         #false)]
    [(boa-constrictor? za) (>= (cage-volume c) 1)]
    [(armadillo? za) (>= (cage-volume c) 3.5)]))

(check-expect (fits? (make-spider 7 .001) (make-cage .1 .1 .1))
              #true)
(check-expect (fits? (make-spider 7 .002) (make-cage .1 .1 .1))
              #false)

(check-expect (fits? (make-elephant 125) (make-cage 8 5 6))
              #true)
(check-expect (fits? (make-elephant 1000) (make-cage 8 5 6))
              #false)

(check-expect (fits? (make-boa-constrictor 3 0.2) (make-cage 1 1 1))
              #true)
(check-expect (fits? (make-boa-constrictor 3 0.2) (make-cage 1 1 .9))
              #false)

(check-expect (fits? (make-armadillo 30 1.5 2) (make-cage 1.6 1.7 1.5))
              #true)
(check-expect (fits? (make-armadillo 30 1.5 2) (make-cage 1 1 1))
              #false)

; Cage -> Number
; Calculate the cage's volume in cubic meters.
(define (cage-volume c)
  (* (cage-width c) (cage-depth c) (cage-height c)))

(check-expect (cage-volume (make-cage 1 2 1))
              2)