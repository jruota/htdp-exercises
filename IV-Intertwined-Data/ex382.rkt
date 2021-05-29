;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex382) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; An FSM is a [List-of 1Transition]

; A 1Transition is a list of three items:
;   (cons FSM-State (cons FSM-State (cons KeyStroke '())))
; Interpretation:
;     The current and the next state as well as the key
;     stroke triggering a state change.

; An FSM-State is a String that specifies a color.

; An XMachine is a nested list of this shape:
;     (cons 'machine (cons `((initial ,FSM-State)) [List-of X1T]))

; An X1T is a nested list of this shape:
;     `(action ((state ,FSM-State) (next ,FSM-State)))

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; XML --------------------------------------------------------------------------

; <machine initial="white">
;   <action state="white" next="black" />
;   <action state="black" next="white" />
; </machine>

; XMachine
'(machine ((initial "white"))
          (action ((state "white") (next "black")))
          (action ((state "black") (next "white"))))