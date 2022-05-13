;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex320b) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; An S-expr is one of: 
; – Number
; – String
; – Symbol
; – [List-of S-expr]

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; S-expr Symbol -> N 
; Count all occurrences of sy in sexp.
(define (count sexp sy)
  (cond
    [(number? sexp) 0]
    [(string? sexp) 0]
    [(symbol? sexp)
     (if (symbol=? sexp sy) 1 0)]
    [else
     (foldl (lambda (x y) (+ (count x sy) y)) 0 sexp)]))

(check-expect (count 23 'world) 0)
(check-expect (count "something" 'world) 0)
(check-expect (count 'world 'world) 1)
(check-expect (count '() 'world) 0)
(check-expect (count 'world 'hello) 0)
(check-expect (count '(world hello) 'hello) 1)
(check-expect (count '(((world) hello) hello) 'hello) 2)

; from ex316.rkt ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Any -> Boolean
; Is a an Atom?
(define (atom? a)
  (or (number? a)
      (string? a)
      (symbol? a)))
