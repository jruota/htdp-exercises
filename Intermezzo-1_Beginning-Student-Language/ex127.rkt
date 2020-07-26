;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex127) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct ball [x y speed-x speed-y])

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(number? (make-ball 1 2 3 4))

; #false, as this is a ball-structure, not a number.

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(ball-speed-y (make-ball (+ 1 2) (+ 3 3) 2 3))

; 3, as this is the value in the speed-y-field.

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(ball-y (make-ball (+ 1 2) (+ 3 3) 2 3))

; 6, as (+ 3 3) is in the y-field and evaluates to 6.

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;(ball-x (make-posn 1 2))

; Signals an error, since ball-x expects a ball-structure, but is given
; a posn.

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;(ball-speed-y 5)

; Signals an error, since ball-speed-y expects a ball-structure, but is given
; a number.