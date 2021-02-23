;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex318) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; An S-expr is one of: 
; – Atom
; – SL
 
; An SL is one of: 
; – '()
; – (cons S-expr SL)

; An Atom is one of: 
; – Number
; – String
; – Symbol 

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; S-expr -> N
; Return the depth of sexpr.
(define (depth sexpr)
  (cond
    [(atom? sexpr) 1]
    [else
     (sl-depth sexpr)]))

(check-expect (depth 3.14)
              1)
(check-expect (depth "hello")
              1)
(check-expect (depth 'world)
              1)
(check-expect (depth '())
              1)
(check-expect (depth '(4 "banana" + ()))
              2)
(check-expect (depth '(((world) hello) hello))
              4)
(check-expect (depth '(4 "banana" + () (((world) hello) hello)))
              5)

; SL -> N
; Return the depth of sl.
(define (sl-depth sl)
  (cond
    [(empty? sl) 1]
    [else
     (max (+ 1 (depth (first sl)))
          (sl-depth (rest sl)))]))

(check-expect (sl-depth '())
              1)
(check-expect (sl-depth '(((world) hello) hello))
              4)
(check-expect (sl-depth (list '() '() '() '() '() '()))
              2)

; Any -> Boolean
; Is a of type atom?
(define (atom? a)
  (or (number? a)
      (string? a)
      (symbol? a)))

(check-expect (atom? 4.321)
              #true)
(check-expect (atom? "hello")
              #true)
(check-expect (atom? '+)
              #true)

(check-expect (atom? (make-posn 1 2))
              #false)
(check-expect (atom? empty-image)
              #false)
(check-expect (atom? '())
              #false)