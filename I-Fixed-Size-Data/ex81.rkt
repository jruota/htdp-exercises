;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex81) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define-struct pitsm [h m s])
; A PITSM (points in time since midnight) is a structure:
;    (make-pitsm Number Number Number)
; The following restrictions apply:
;     – first number 0 <= n <= 23
;     – second number 0 <= n <= 59
;     – third number 0 <= n <= 59
; Interpretation:
;     Hours, minutes and seconds since midnight.

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; PITSM -> Number
; Produce the number of seconds that have passed
; since midnight.
(define (time->seconds pitsm)
  (+ (* 3600 (pitsm-h pitsm))
     (* 60 (pitsm-m pitsm))
     (pitsm-s pitsm)))

(check-expect (time->seconds (make-pitsm 12 30 2))
              45002)