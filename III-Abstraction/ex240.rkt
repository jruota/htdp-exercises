;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex240) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define-struct layer [stuff])
; A [Layer STUFF] is a structure:
;     (make-layer STUFF)

; An LStr is one of: 
; – String
; – (make-layer LStr)

; An LNum is one of: 
; – Number
; – (make-layer LNum)

; An [LSTUFF] is one of:
; – STUFF
; – [Layer STUFF]

; An LStr-1 is one of:
; – String
; – [Layer String]

; An LNum-1 is one of:
; – Number
; – [Layer Number]

; DATA EXAMPLES ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; LStr
"hello"
(make-layer "world")
(make-layer (make-layer "string"))

; LNum
12
(make-layer 12)
(make-layer (make-layer 12))