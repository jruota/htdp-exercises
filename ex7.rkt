;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex7) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define sunny #true)
(define friday #false)

(and (not sunny) friday)

; Because sunny is #true and therefore (not sunny) is #false
; (we are asked to compute if sunny is #false - not sunny) would result
; in #true when sunny is #false), the and expression results in #false and
; friday is never even evaluated.