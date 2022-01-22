;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex527) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define DELTA 0.000001)

;(define THRESHOLD 10)
;
;(define LEFT-ANGLE 0.15)
;(define LEFT-LENGTH 2/3)
;(define RIGHT-ANGLE 0.2)
;(define RIGHT-LENGTH 4/5)

(define THRESHOLD 10)

(define LEFT-ANGLE 0.5)
(define LEFT-LENGTH 2/3)
(define RIGHT-ANGLE 0.6)
(define RIGHT-LENGTH 4/5)

(define WIDTH 800)
(define HEIGHT (* 9/16 WIDTH))
(define TEST-SCENE (empty-scene WIDTH HEIGHT))
(define COLOR "red")

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Image Number Number Number Number -> Image
; Draw a Savannah tree in img, starting with a line
; with it's base point at (x, y), a length of len
; and at an angle of ang.
; Constraint:
;     The argument ang should be in degrees.
(define (add-savannah img x y len ang)
  (cond
    [(too-small? len) img]
    [else
     (local ((define RADIANS (deg-to-rad ang))
             (define x13 (+ x (* 1/3 len (cos RADIANS))))
             (define y13 (- y (* 1/3 len (sin RADIANS))))
             (define x23 (+ x (* 2/3 len (cos RADIANS))))
             (define y23 (- y (* 2/3 len (sin RADIANS))))
             (define x33 (+ x (* 1 len (cos RADIANS))))
             (define y33 (- y (* 1 len (sin RADIANS))))
             (define left-angle (rad-to-deg (+ RADIANS LEFT-ANGLE)))
             (define left-length (* len LEFT-LENGTH))
             (define right-angle (rad-to-deg (- RADIANS RIGHT-ANGLE)))
             (define right-length (* len RIGHT-LENGTH))
             (define scene1 (scene+line img x y x33 y33 COLOR))
             (define scene2
               (add-savannah scene1 x13 y13 left-length left-angle)))
       ; – IN –
       (add-savannah scene2 x23 y23 right-length right-angle))]))


; Number -> Boolean
; Is n less than THRESHOLD?
(define (too-small? n)
  (< n THRESHOLD))

(check-expect (too-small? (+ THRESHOLD 1))
              #false)
(check-expect (too-small? THRESHOLD)
              #false)
(check-expect (too-small? (- THRESHOLD 1))
              #true)

; Number -> Number
; Return the angle rad (in radians)
; in degrees.
(define (rad-to-deg rad)
  (* rad (/ 360 (* 2 pi))))

(check-expect (rad-to-deg 0) 0)
(check-within (rad-to-deg (* 1/4 pi)) 45 DELTA)
(check-within (rad-to-deg (* 1/2 pi)) 90 DELTA)

; from ex526.rkt ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Number -> Number
; Return the angle deg (in degrees)
; in radians.
(define (deg-to-rad deg)
  (* deg (/ (* 2 pi) 360)))

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(add-savannah TEST-SCENE (* 3/9 WIDTH) HEIGHT (* 5/12 HEIGHT) 90)
