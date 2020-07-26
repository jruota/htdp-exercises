;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex126) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct point [x y z])
(define-struct none  [])

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; From "Structure Type Definitions":

; Simply put, the use of define-struct extends the universe of values. To start
; with, it now also contains structures, which compound several values into one.
; When a program contains a define-struct definition, its evaluation modifies
; the definition of values:
;
;     A value is one of: a number, a Boolean, a string, an image,
;     or a structure value:
;
;         (make-c _value-1 ... _value-n)
;
;     assuming a structure type c is defined.

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(make-point 1 2 3)

; "(make-point 1 2 3)" is a structure value.

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(make-point (make-point 1 2 3) 4 5)

; "(make-point (make-point 1 2 3) 4 5)" is a structure value containing a
; structure and numbers.

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(make-point (+ 1 2) 3 4)

; "(make-point 3 3 4)" is a structure value.

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(make-none)

; "(make-none)" is a structure value with no fields.

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(make-point (point-x (make-point 1 2 3)) 4 5)

; "(make-point 1 4 5)" is a structure value.
