;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex230) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define-struct fsm [initial transitions final])
; An FSM is a structure: 
;   (make-fsm FSM-State LOT FSM-State)

; A LOT is one of: 
; – '() 
; – (cons Transition LOT)

(define-struct transition [current key next])
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

;(define-struct fs [fsm current])
;; A SimulationState is a structure: 
;;   (make-fs FSM FSM-State)

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define AA "white")
(define BB "yellow")
(define DD "green")
(define ER "red")

(define FSM (make-fsm AA
                      (list (make-transition AA "a" BB)
                            (make-transition BB "b" BB)
                            (make-transition BB "c" BB)
                            (make-transition BB "d" DD))
                      DD))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; FSM -> FSM
; Run a finite state machine.
(define (fsm-simulate fsm)
  (big-bang fsm
    [to-draw state-as-colored-square]
    [on-key find-next-state]
    [stop-when final-state-or-error? state-as-colored-square]))

; FSM -> Image 
; Render the current world state as a colored square. 
(define (state-as-colored-square fsm)
  (square 100 "solid" (fsm-initial fsm)))

(check-expect (state-as-colored-square FSM)
              (square 100 "solid" "white"))
(check-expect (state-as-colored-square (make-fsm BB
                                                 (fsm-transitions FSM)
                                                 DD))
              (square 100 "solid" "yellow"))

; FSM KeyEvent -> FSM
; Find the next state from fsm and ke.
(define (find-next-state fsm ke)
  (make-fsm
   (find (fsm-initial fsm) ke (fsm-transitions fsm))
   (fsm-transitions fsm)
   (fsm-final fsm)))

(check-expect (find-next-state FSM "a")
              (make-fsm BB
                        (fsm-transitions FSM)
                        DD))

; FSM-State KeyEvent LOT -> FSM-State
; Find the next state depending on the current state
; of fsm and the key event ke.
(define (find fsm-s ke lot)
  (cond
    [(empty? lot)
     ER]
    [(cons? lot)
     (if (and (string=? (transition-current (first lot))
                        fsm-s)
              (key=? ke (transition-key (first lot))))
         (transition-next (first lot))
         (find fsm-s ke (rest lot)))]))

(check-expect (find AA "a" (fsm-transitions FSM))
              BB)
(check-expect (find AA "v" (fsm-transitions FSM))
              ER)

(check-expect (find BB "b" (fsm-transitions FSM))
              BB)
(check-expect (find BB "c" (fsm-transitions FSM))
              BB)
(check-expect (find BB "d" (fsm-transitions FSM))
              DD)
(check-expect (find BB "e" (fsm-transitions FSM))
              ER)

; FSM -> Boolean
; Has the finite state machine reached its final
; state or has an error occured?
(define (final-state-or-error? fsm)
  (or
   (string=? (fsm-initial fsm)
             (fsm-final fsm))
   (string=? (fsm-initial fsm)
             ER)))

(check-expect (final-state-or-error? FSM)
              #false)
(check-expect (final-state-or-error? (make-fsm
                                      BB
                                      (fsm-transitions FSM)
                                      DD))
              #false)
(check-expect (final-state-or-error? (make-fsm
                                      DD
                                      (fsm-transitions FSM)
                                      DD))
              #true)
(check-expect (final-state-or-error? (make-fsm
                                      ER
                                      (fsm-transitions FSM)
                                      DD))
              #true)