;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex227) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A BW-Machine (black and white machine) is a list
;     (list (make-transition "black" "white")
;           (make-transition "white" "black"))
; Interpretation:
;     A collection of all valid state changes for a BW-Machine.
 
(define-struct transition [current next])
; A Transition is a structure:
;   (make-transition FSM-State FSM-State)

; An FSM-State is one of:
;     – "black"
;     – "white"
; Interpretation:
;     The two valid states a BW-Machine can take
;     in reaction to key-strokes.
