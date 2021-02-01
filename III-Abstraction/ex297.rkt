;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex297) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A Shape is a function: 
;   [Posn -> Boolean]
; interpretation if s is a shape and p a Posn, (s p) 
; produces #true if p is in s, #false otherwise

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Number Number Number -> Shape 
; creates a representation for a circle of radius r
; located at (center-x, center-y) 
(define (mk-circle center-x center-y r)
  ; [Posn -> Boolean]
  (lambda (p)
    (local (; Number Number Posn -> Number
            ; Calculate the distance between the Posn p
            ; and the circles center at (x, y).
            (define (distance-between x y p)
              (sqrt
               (+ (sqr (- x (posn-x p)))
                  (sqr (- y (posn-y p)))))))
      (<= (distance-between center-x center-y p) r))))

(check-expect
  (inside? (mk-circle 3 4 5) (make-posn 0 0)) #true)
(check-expect
  (inside? (mk-circle 3 4 5) (make-posn 0 9)) #false)
(check-expect
  (inside? (mk-circle 3 4 5) (make-posn -1 3)) #true)

; Shape Posn -> Boolean
; Is the point p inside the shape s?
(define (inside? s p)
  (s p))

