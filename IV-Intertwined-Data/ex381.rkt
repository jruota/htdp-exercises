;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex381) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; The definitions of XMachine and X1T use quote, which is highly inappropriate
; for novice program designers. Rewrite them first to use list and then cons.

; An XMachine is a nested list of this shape:
;     (list 'machine (list (list 'initial FSM-State)) [List-of X1T]))

; An X1T is a nested list of this shape:
;     (list 'action (list (list 'state FSM-State) (list 'next FSM-State)))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

; An XMachine is a nested list of this shape:
;     (cons 'machine
;           (cons (cons (cons 'initial (cons FSM-State '())) '())
;                 [List-of X1T]))

; An X1T is a nested list of this shape:
;     (cons 'action
;           (cons
;            (cons (cons 'state (cons FSM-State '()))
;                  (cons (cons 'next (cons FSM-State '()))
;                        '()))
;            '()))

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define xm0
  '(machine ((initial "red"))
            (action ((state "red") (next "green")))
            (action ((state "green") (next "yellow")))
            (action ((state "yellow") (next "red")))))

(define xm1
  (cons
   'machine
   (cons
    `((initial ,"red"))
    (cons
     `(action ((state ,"red") (next ,"green")))
     (cons
      `(action ((state ,"green") (next ,"yellow")))
      (cons
       `(action ((state ,"yellow") (next ,"red"))) '()))))))

(define xm2
  (list 'machine (list (list 'initial "red"))
        (list 'action (list (list 'state "red") (list 'next "green")))
        (list 'action (list (list 'state "green") (list 'next "yellow")))
        (list 'action (list (list 'state "yellow") (list 'next "red")))))

(define xm3
  (cons 'machine
        (cons (cons (cons 'initial (cons "red" '())) '())
              (cons
               (cons 'action
                     (cons
                      (cons (cons 'state (cons "red" '()))
                            (cons (cons 'next (cons "green" '()))
                                  '()))
                      '()))
               (cons
                (cons 'action
                     (cons
                      (cons (cons 'state (cons "green" '()))
                            (cons (cons 'next (cons "yellow" '()))
                                  '()))
                      '()))
                (cons
                 (cons 'action
                     (cons
                      (cons (cons 'state (cons "yellow" '()))
                            (cons (cons 'next (cons "red" '()))
                                  '()))
                      '()))
                 '()))))))

(check-expect `(action ((state ,"red") (next ,"green")))
              (list 'action (list (list 'state "red") (list 'next "green"))))

(check-expect `(action ((state ,"red") (next ,"green")))
              (cons 'action
                    (cons
                     (cons (cons 'state (cons "red" '()))
                           (cons (cons 'next (cons "green" '()))
                                 '()))
                     '())))

(check-expect xm0 xm1)
(check-expect xm0 xm2)
(check-expect xm0 xm3)