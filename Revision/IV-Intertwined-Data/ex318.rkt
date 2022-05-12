;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex318) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; An S-expr is one of: 
; – Atom
; – SL

; An Atom is one of: 
; – Number
; – String
; – Symbol

; An SL is one of: 
; – '()
; – (cons S-expr SL)

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; S-expr -> Number
; Determine the depth of sexpr.
; An Atom has a depth of 1.
; The depth of a list of S-expressions
; is the maximum depth of its items plus 1.
(define (depth sexpr)
  (cond
    [(atom? sexpr) 1]
    [else
     (sl-depth sexpr)]))

(check-expect (depth 23) 1)
(check-expect (depth "twenty-three") 1)
(check-expect (depth 'symbol) 1)

(check-expect (depth '()) 1)

(define sl1 (list 1 "two" 'three))
(define sl2 (list sl1 sl1))
(define sl3 (list (list sl1 sl2) sl1))
(define sl4 (list (list (list (list sl1)))))

(check-expect (depth sl1) 2)
(check-expect (depth sl2) 3)
(check-expect (depth sl3) 5)
(check-expect (depth sl4) 6)

; SL -> Number
; The depth of a list of S-expressions
; is the maximum depth of its items plus 1.
(define (sl-depth sl)
  (cond
    [(empty? sl) 1]
    [else
     (max (+ 1 (depth (first sl)))
          (sl-depth (rest sl)))]))

(check-expect (sl-depth '()) 1)
(check-expect (sl-depth sl1) 2)
(check-expect (sl-depth sl2) 3)
(check-expect (sl-depth sl3) 5)
(check-expect (sl-depth sl4) 6)

; from ex316.rkt ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Any -> Boolean
; Is a an Atom?
(define (atom? a)
  (or (number? a)
      (string? a)
      (symbol? a)))
