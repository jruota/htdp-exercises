;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex72) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct phone [area number])
; A Phone is a Structure:
;     (make-phone Number String)
; Interpretation:
;     Combines the area code and the local number
;     of a phone number.

; NOTE
; see https://en.wikipedia.org/wiki/North_American_Numbering_Plan
; END NOTE
(define-struct phone# [area switch num])
; A Phone# is a Structure:
;     (make-phone Number Number Number)
; The following constraints apply:
;     – area - [2-9] for the first digit,
;              [0-9] for the second and third digits
;     – switch - [2–9] for the first digit,
;                [0–9] for the second and third digits 
;     – num - [0–9] for each of the four digits
; Interpretation:
;     An US phone number.