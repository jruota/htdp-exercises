;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex229) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; An FSM is one of:
;   – '()
;   – (cons Transition FSM)
 
(define-struct ktransition [current key next])
; A Transition is a structure:
;   (make-ktransition FSM-State KeyEvent FSM-State)
 
; FSM-State is a Color.

; A Color is one of: 
; — "white"
; — "yellow"
; — "orange"
; — "green"
; — "red"
; — "blue"
; — "black"

; interpretation An FSM represents the transitions that a
; finite state machine can take from one state to another 
; in reaction to keystrokes

(define-struct fs [fsm current])
; A SimulationState is a structure: 
;   (make-fs FSM FSM-State)

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define AA "white")
(define BB "yellow")
(define DD "green")
(define ER "red")

(define fsm-ExpectsToSee
  (list (make-ktransition AA "a" BB)
        (make-ktransition BB "b" BB)
        (make-ktransition BB "c" BB)
        (make-ktransition BB "d" DD)))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; FSM FSM-State -> SimulationState
; match the keys pressed with the given FSM 
(define (simulate an-fsm s0)
  (big-bang (make-fs an-fsm s0)
    [to-draw state-as-colored-square]
    [on-key find-next-state]))

; SimulationState -> Image 
; renders current world state as a colored square 
(define (state-as-colored-square an-fsm)
  (square 100 "solid" (fs-current an-fsm)))

(check-expect (state-as-colored-square (make-fs fsm-ExpectsToSee "white"))
              (square 100 "solid" "white"))
(check-expect (state-as-colored-square (make-fs fsm-ExpectsToSee "yellow"))
              (square 100 "solid" "yellow"))

; SimulationState KeyEvent -> SimulationState
; finds the next state from an-fsm and ke
(define (find-next-state an-fsm ke)
  (make-fs
    (fs-fsm an-fsm)
    (find (fs-fsm an-fsm) ke (fs-current an-fsm))))

(check-expect (find-next-state (make-fs fsm-ExpectsToSee AA) "a")
              (make-fs fsm-ExpectsToSee BB))

(check-expect (find-next-state (make-fs fsm-ExpectsToSee BB) "b")
              (make-fs fsm-ExpectsToSee BB))

(check-expect (find-next-state (make-fs fsm-ExpectsToSee BB) "c")
              (make-fs fsm-ExpectsToSee BB))

(check-expect (find-next-state (make-fs fsm-ExpectsToSee BB) "a")
              (make-fs fsm-ExpectsToSee ER))

(check-expect (find-next-state (make-fs fsm-ExpectsToSee BB) "d")
              (make-fs fsm-ExpectsToSee DD))

(check-expect (find-next-state (make-fs fsm-ExpectsToSee AA) " ")
              (make-fs fsm-ExpectsToSee ER))

; FSM KeyEvent FSM-State -> FSM-State
; finds the state representing current in transitions
; and retrieves the next field depending on ke
(define (find transitions ke current)
  (cond
    [(empty? transitions)
     ER]
    [(cons? transitions)
     (if (and (string=? (ktransition-current (first transitions))
                        current)
              (key=? ke (ktransition-key (first transitions))))
         (ktransition-next (first transitions))
         (find (rest transitions) ke current))]))

(check-expect (find fsm-ExpectsToSee "a" AA)
              BB)
(check-expect (find fsm-ExpectsToSee "b" BB)
              BB)
(check-expect (find fsm-ExpectsToSee "c" BB)
              BB)
(check-expect (find fsm-ExpectsToSee "a" BB)
              ER)

(check-expect (find fsm-ExpectsToSee "d" BB)
              DD)
(check-expect (find fsm-ExpectsToSee "d" AA)
              ER)
