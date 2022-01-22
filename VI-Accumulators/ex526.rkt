;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex526) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define DELTA 0.000001)

(define CENTER (make-posn 200 200))
(define RADIUS 200) ; the radius in pixels 

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; for testing only -------------------------------------------------------------

; Posn Posn -> Boolean
; Are the coordinates of posn1 and posn2
; within DELTA of each other?
(define (compare-posn posn1 posn2)
  (local ((define x1 (posn-x posn1))
          (define y1 (posn-y posn1))
          (define x2 (posn-x posn2))
          (define y2 (posn-y posn2)))
    ; – IN –
    (and (<= (abs (- x1 x2)) DELTA)
         (<= (abs (- y1 y2)) DELTA))))

(check-expect (compare-posn (make-posn 1 2) (make-posn 1 2))
              #true)
(check-expect (compare-posn (make-posn 1 2) (make-posn (+ 1 DELTA) 2))
              #true)
(check-expect (compare-posn (make-posn 1 2) (make-posn (+ 1 DELTA DELTA) 2))
              #false)

; ------------------------------------------------------------------------------

; Number -> Posn
; Determine the point on the circle with CENTER 
; and RADIUS whose angle is factor in degrees.
; Constraint:
;     The parameter factor is supposed to be
;     in the range [0, 360].
 
; examples
; what are the x and y coordinates of the desired 
; point, when given: 120/360, 240/360, 360/360
 
(define (circle-pt factor)
  (local ((define RADIANS (deg-to-rad factor))
          (define POLAR (make-polar RADIUS RADIANS))
          (define X (real-part POLAR))
          (define Y (imag-part POLAR)))
    ; – IN –
    (make-posn (+ X RADIUS) (+ Y RADIUS))))

(check-expect (compare-posn (circle-pt 0) (make-posn 400 200))
              #true)
(check-expect (compare-posn (circle-pt 90) (make-posn 200 400))
              #true)
(check-expect (compare-posn (circle-pt 180) (make-posn 0 200))
              #true)
(check-expect (compare-posn (circle-pt 270) (make-posn 200 0))
              #true)
(check-expect (compare-posn (circle-pt 360) (circle-pt 0))
              #true)

(check-expect (compare-posn
               (circle-pt 120)
               (make-posn (* 200 (+ 1 (cos (* 120 (/ (* 2 pi) 360)))))
                          (* 200 (+ 1 (sin (* 120 (/ (* 2 pi) 360)))))))
              #true)
(check-expect (compare-posn
               (circle-pt 240)
               (make-posn (* 200 (+ 1 (cos (* 240 (/ (* 2 pi) 360)))))
                          (* 200 (+ 1 (sin (* 240 (/ (* 2 pi) 360)))))))
              #true)
(check-expect (compare-posn
               (circle-pt 360)
               (make-posn (* 200 (+ 1 (cos (* 360 (/ (* 2 pi) 360)))))
                          (* 200 (+ 1 (sin (* 360 (/ (* 2 pi) 360)))))))
              #true)

; Number -> Posn
; Determine the point on the circle with CENTER 
; and RADIUS whose angle is factor in degrees.
; Constraint:
;     The parameter factor is supposed to be
;     in the range [0, 360].
(define (circle-pt.v2 factor)
  (local ((define RADIANS (deg-to-rad factor))
          (define X (+ 1 (cos RADIANS)))
          (define Y (+ 1 (sin RADIANS))))
    ; – IN –
    (make-posn (* X RADIUS) (* Y RADIUS))))

(check-expect (compare-posn (circle-pt.v2 0) (make-posn 400 200))
              #true)
(check-expect (compare-posn (circle-pt.v2 90) (make-posn 200 400))
              #true)
(check-expect (compare-posn (circle-pt.v2 180) (make-posn 0 200))
              #true)
(check-expect (compare-posn (circle-pt.v2 270) (make-posn 200 0))
              #true)
(check-expect (compare-posn (circle-pt.v2 360) (circle-pt.v2 0))
              #true)

(check-expect (compare-posn
               (circle-pt.v2 120)
               (make-posn (* 200 (+ 1 (cos (* 120 (/ (* 2 pi) 360)))))
                          (* 200 (+ 1 (sin (* 120 (/ (* 2 pi) 360)))))))
              #true)
(check-expect (compare-posn
               (circle-pt.v2 240)
               (make-posn (* 200 (+ 1 (cos (* 240 (/ (* 2 pi) 360)))))
                          (* 200 (+ 1 (sin (* 240 (/ (* 2 pi) 360)))))))
              #true)
(check-expect (compare-posn
               (circle-pt.v2 360)
               (make-posn (* 200 (+ 1 (cos (* 360 (/ (* 2 pi) 360)))))
                          (* 200 (+ 1 (sin (* 360 (/ (* 2 pi) 360)))))))
              #true)

; Number -> Number
; Return the angle deg (in degrees)
; in radians.
(define (deg-to-rad deg)
  (* deg (/ (* 2 pi) 360)))

(check-expect (deg-to-rad 0) 0)
(check-within (deg-to-rad 45) (* 1/4 pi) DELTA)
(check-within (deg-to-rad 90) (* 1/2 pi) DELTA)
