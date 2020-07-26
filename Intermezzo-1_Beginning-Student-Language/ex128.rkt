;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex128) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(check-member-of "green" "red" "yellow" "grey")

; Fails because none of "red", "yellow" or "grey" are equal to "green".

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(check-within (make-posn #i1.0 #i1.1)
              (make-posn #i0.9 #i1.2)  0.01)

; Fails, because the x-field and y-field are within 0.1 of the expected value,
; and not within 0.01.

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(check-range #i0.9 #i0.6 #i0.8)

; Fails, because 0.9 is not within the range [0.6, 0.8].

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(check-random (make-posn (random 3) (random 9))
              (make-posn (random 9) (random 3)))

; Fails, because random is called with different arguments and thus (possibly)
; yields different numbers.

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(check-satisfied 4 odd?)

; Fails, because 4 is even.