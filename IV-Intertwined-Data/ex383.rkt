;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex383) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; An FSM is a [List-of 1Transition]

; A 1Transition is a list of three items:
;   (cons FSM-State (cons FSM-State '()))
; Interpretation:
;     The current and the next state as well as the key
;     stroke triggering a state change.

; An FSM-State is a String that specifies a color.

; An XMachine is a nested list of this shape:
;     (cons 'machine (cons `((initial ,FSM-State)) [List-of X1T]))

; An X1T is a nested list of this shape:
;     `(action ((state ,FSM-State) (next ,FSM-State)))

; An Xexpr is a list: 
; – (cons Symbol Body)
; – (cons Symbol (cons [List-of Attribute] Body))
; where Body is short for [List-of Xexpr]

; An Attribute is a list of two items:
;   (cons Symbol (cons String '()))

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; XMachine
(define fsm-bw
  '(machine ((initial "white"))
            (action ((state "white") (next "black")))
            (action ((state "black") (next "white")))))

(define xm0
  '(machine ((initial "red"))
     (action ((state "red") (next "green")))
     (action ((state "green") (next "yellow")))
     (action ((state "yellow") (next "red")))))

(define fsm-traffic
  '(("red" "green") ("green" "yellow") ("yellow" "red")))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; XMachine -> FSM-State 
; interprets the given configuration as a state machine 
(define (simulate-xmachine xm)
  (simulate (xm-state0 xm) (xm->transitions xm)))
 
; XMachine -> FSM-State 
; extracts and translates the transition table from xm0
(define (xm-state0 xm0)
  (find-attr (xexpr-attr xm0) 'initial))

(check-expect (xm-state0 xm0) "red")
 
; XMachine -> [List-of 1Transition]
; extracts the transition table from xm
(define (xm->transitions xm)
  (local (; X1T -> 1Transition
          (define (xaction->action xa)
            (list (find-attr (xexpr-attr xa) 'state)
                  (find-attr (xexpr-attr xa) 'next))))
    (map xaction->action (xexpr-content xm))))

(check-expect (xm->transitions xm0) fsm-traffic)

; FSM FSM-State -> FSM-State 
; matches the keys pressed by a player with the given FSM 
(define (simulate state0 transitions)
  (big-bang state0 ; FSM-State
    [to-draw
      (lambda (current)
        (square 100 "solid" current))]
    [on-key
      (lambda (current key-event)
        (find transitions current))]))
 
; [X Y] [List-of [List X Y]] X -> Y
; finds the matching Y for the given X in alist
(define (find alist x)
  (local ((define fm (assoc x alist)))
    (if (cons? fm) (second fm) (error "not found"))))

; from ex369.rkt ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; [List-of Attribute] Symbol -> SorF
; If loa associates s with a string,
; return the string. Otherwise return #false.
(define (find-attr loa s)
  (local ((define RES (assq s loa)))
    (if (boolean? RES)
        #false
        (second RES))))

; from ex366.rkt ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Xexpr -> [List-of Attribute]
; Retrieve the list of attributes of xe.
(define (xexpr-attr xe)
  (local ((define optional-loa+content (rest xe)))
    (cond
      [(empty? optional-loa+content) '()]
      [else
       (local ((define loa-or-x
                 (first optional-loa+content)))
         (if (list-of-attributes? loa-or-x)
             loa-or-x
             '()))])))

; Xexpr -> [List-of Xexpr]
; Retrieve the list of content elements of xe.
(define (xexpr-content xe)
  (local ((define optional-loa+content (rest xe)))
    (cond
      [(empty? optional-loa+content) '()]
      [else
       (local ((define loa-or-content (first optional-loa+content)))
         (if (list-of-attributes? loa-or-content)
             (rest optional-loa+content)
             optional-loa+content))])))

; [List-of Attribute] or Xexpr -> Boolean
; Is x a list of attributes?
(define (list-of-attributes? x)
  (cond
    [(empty? x) #true]
    [else
     (local ((define possible-attribute (first x)))
       (cons? possible-attribute))]))