;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex380) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

; DATA DEFINTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; An FSM is a [List-of 1Transition]

; A 1Transition is a list of three items:
;   (cons FSM-State (cons FSM-State (cons KeyStroke '())))
; Interpretation:
;     The current and the next state as well as the key
;     stroke triggering a state change.

; An FSM-State is a String that specifies a color.

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; data examples 
(define fsm-traffic
  '(("red" "green" "c") ("green" "yellow" "a") ("yellow" "red" "b")))

(define COLOR "black")
(define SIZE 20)

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; What else do you need to change to get the complete program to work?
; Which part of the design recipe provides the answer(s)?

; The key-handling function needs to be changed to check for the right
; key-event.
; The fourth or template step provides the answer(s). Although here the value
; is not unpacked completely and instead of a cond an if is used.

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; FSM-State FSM -> FSM-State 
; matches the keys pressed by a player with the given FSM 
(define (simulate state0 transitions)
  (big-bang state0 ; FSM-State
    [to-draw
      (lambda (current)
        (overlay
         (text current SIZE COLOR)
         (square 100 "solid" current)))]
    [on-key
      (lambda (current key-event)
        (local ((define STT-CHNG (assoc current transitions)))
          (if (key=? key-event (third STT-CHNG))
              (find transitions current)
              current)))]))
 
; [X Y] [List-of [List X Y]] X -> Y
; finds the matching Y for the given X in alist
(define (find alist x)
  (local ((define fm (assoc x alist)))
    (if (cons? fm) (second fm) (error "not found"))))

(check-expect (find fsm-traffic "yellow")
              "red")
(check-error (find fsm-traffic "orange")
             "not found")