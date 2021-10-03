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

; ExpectsToSee is one of:
; – AA
; – BB
; – DD 
; – ER

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
; Does an-fsm recognize the given string?
(define (fsm-match? an-fsm a-string)
  (local (; FSM-State [List-of 1String] -> Boolean
          ; Starting from current state of an-fsm, are
          ; the transitions represented by remaining legal,
          ; i.e. do they lead to the final state of an-fsm?
          (define (legal-transitions? current remaining)
            (cond
              [(empty? remaining) #true]
              [else
               (local ((define next-state
                         (next an-fsm current (first remaining))))
                 ; – IN –
                 (and (legal-state? an-fsm next-state)
                      (legal-transitions? next-state (rest remaining))))])))
    ; – IN –
    (legal-transitions? (fsm-initial an-fsm) (explode a-string))))

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

; FSM String String -> String
; Find the next state of an-fsm
; based on its current state and
; the key.
(define (next an-fsm current key)
  (local ((define transitions (fsm-transitions an-fsm))

          ; FSM-State [List-of 1Transition] -> FSM-State
          ; Do the actual work.
          (define (find-next current0 transitions0)
            (cond
              [(empty? transitions0)
               ER]
              [else
               (if (and (string=? (transition-current (first transitions0))
                                  current)
                        (string=? (transition-key (first transitions0))
                                  key))
                   (transition-next (first transitions0))
                   (find-next current0 (rest transitions0)))])))
    ; – IN –
    (find-next (fsm-initial an-fsm) transitions)))


(check-expect (next fsm-a-bc*-d AA "a")
              BC)
(check-expect (next fsm-a-bc*-d BC "b")
              BC)
(check-expect (next fsm-a-bc*-d BC "c")
              BC)
(check-expect (next fsm-a-bc*-d BC "d")
              DD)

(check-expect (next fsm-a-bc*-d BC "a")
              ER)
(check-expect (next fsm-a-bc*-d AA "d")
              ER)
(check-expect (next fsm-a-bc*-d BC "r")
              ER)

; FSM FSM-State -> Boolean
; Is current a legal state of an-fsm?
(define (legal-state? an-fsm current)
  (local ((define transitions (fsm-transitions an-fsm))
          (define final (fsm-final an-fsm))

          ; [List-of Transition] -> Boolean
          ; Do the actual work.
          (define (legal? lot)
            (cond
              [(empty? lot) #false]
              [else
               (if (or (string=? (transition-current (first lot)) current)
                       (string=? final current))
                   #true
                   (legal? (rest lot)))])))
    ; – IN –
    (legal? transitions)))

(check-expect (legal-state? fsm-a-bc*-d AA)
              #true)
(check-expect (legal-state? fsm-a-bc*-d BC)
              #true)
(check-expect (legal-state? fsm-a-bc*-d DD)
              #true)
(check-expect (legal-state? fsm-a-bc*-d ER)
              #false)