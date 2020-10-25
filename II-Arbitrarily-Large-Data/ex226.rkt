;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex226) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; An FSM is one of:
;   – '()
;   – (cons Transition FSM)
 
(define-struct transition [current next])
; A Transition is a structure:
;   (make-transition FSM-State FSM-State)
 
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

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define ERR-MSG "'state=?' has to be given two FSM-States")

(define FSM-STATE-COLLECTION
  (list
   "white"
   "yellow"
   "orange"
   "green"
   "red"
   "blue"
   "black"))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; FSM-State FSM-State -> Boolean
; Are the states s1 and s2 equal?
(define (state=? s1 s2)
  (cond
    [(and (fsm-state? s1)
          (fsm-state? s2))
     (equal? s1 s2)]
    [else
     (error ERR-MSG)]))

(check-expect (state=? "white" "yellow")
              #false)
(check-expect (state=? "orange" "orange")
              #true)
(check-error (state=? "brown" "green")
             ERR-MSG)
(check-error (state=? "red" 12)
             ERR-MSG)

; FSM-State -> Boolean
; Is s a valid FSM-State?
(define (fsm-state? s)
  (member? s FSM-STATE-COLLECTION))

(check-expect (fsm-state? "blue")
              #true)
(check-expect (fsm-state? "purple")
              #false)
              