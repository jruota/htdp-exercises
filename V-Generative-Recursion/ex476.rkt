;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex476) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define-struct transition [current key next])
(define-struct fsm [initial transitions final])
 
; An FSM is a structure:
;   (make-fsm FSM-State [List-of 1Transition] FSM-State)
; A 1Transition is a structure:
;   (make-transition FSM-State 1String FSM-State)
; An FSM-State is String.

(define AA "start, expect an 'a'")
(define BC "expect 'b', 'c', or 'd'")
(define DD "finished")
(define ER "error, illegal key")

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define fsm-a-bc*-d
  (make-fsm
   AA
   (list (make-transition AA "a" BC)
         (make-transition BC "b" BC)
         (make-transition BC "c" BC)
         (make-transition BC "d" DD))
   DD))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; FSM String -> Boolean 
; Does an-fsm recognize the given string,
; i.e. return #true when a-string causes
; the finite state machine an-fsm to transition
; from an initial state to a final state.
(define (fsm-match? an-fsm a-string)
  (local (; FSM-State [List-of 1String] -> Boolean
          ; Starting with current, are the state
          ; transitions specified by remaining legal?
          (define (legal-transitions? current remaining)
            (cond
              [(empty? remaining)
               (equal? current (fsm-final an-fsm))]
              [else
               (local ((define next-state
                         (next an-fsm current (first remaining))))
                 ; – IN –
                 (if (string=? next-state ER)
                     #false
                     (legal-transitions? next-state (rest remaining))))])))
    ; – IN –
    (cond
      [(string=? "" a-string) #true]
      [else
       (legal-transitions? (fsm-initial an-fsm) (explode a-string))])))

(check-expect (fsm-match? fsm-a-bc*-d "ad")
              #true)
(check-expect (fsm-match? fsm-a-bc*-d "abd")
              #true)
(check-expect (fsm-match? fsm-a-bc*-d "abcd")
              #true)
(check-expect (fsm-match? fsm-a-bc*-d "acbd")
              #true)
(check-expect (fsm-match? fsm-a-bc*-d "acbbccbcccbcbcbcbd")
              #true)

(check-expect (fsm-match? fsm-a-bc*-d "")
              #true)

(check-expect (fsm-match? fsm-a-bc*-d "da")
              #false)
(check-expect (fsm-match? fsm-a-bc*-d "aa")
              #false)
(check-expect (fsm-match? fsm-a-bc*-d "d")
              #false)
(check-expect (fsm-match? fsm-a-bc*-d "abcf")
              #false)

; FSM FSM-State 1String -> FSM-State
; Return the next state of an-fsm in the
; state current when given key.
(define (next an-fsm current key)
  (local (; [List-of 1Transition] -> FSM-State
          ; Find the next state of an-fsm from
          ; the list of transitions lo1t given
          ; the current state and key.
          (define (find lo1t)
            (cond
              [(empty? lo1t) ER]
              [else
               (local ((define first-transition (first lo1t)))
                 ; – IN –
                 (if (and (string=? current
                                    (transition-current first-transition))
                          (string=? key (transition-key first-transition)))
                     (transition-next first-transition)
                     (find (rest lo1t))))])))
    ; – IN –
    (find (fsm-transitions an-fsm))))

(check-expect (next fsm-a-bc*-d AA "a") BC)
(check-expect (next fsm-a-bc*-d BC "b") BC)
(check-expect (next fsm-a-bc*-d BC "c") BC)
(check-expect (next fsm-a-bc*-d BC "d") DD)

(check-expect (next fsm-a-bc*-d AA "b") ER)
(check-expect (next fsm-a-bc*-d AA "c") ER)
(check-expect (next fsm-a-bc*-d AA "d") ER)
(check-expect (next fsm-a-bc*-d AA "j") ER)

(check-expect (next fsm-a-bc*-d BC "a") ER)
(check-expect (next fsm-a-bc*-d BC "k") ER)
