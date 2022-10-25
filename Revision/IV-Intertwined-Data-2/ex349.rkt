;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex349) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

; A BSL-expr is one of:
; – Number
; – Add
; – Mul
; Interpretation:
;     Represents BSL expressions.

(define-struct add [left right])
; An Add is a structure:
;     (make-add BSL-expr BSL-expr)
; Interpretation:
;     Represents an addition expression.

(define-struct mul [left right])
; A Mul is a structure:
;     (make-mul BSL-expr BSL-expr)
; Interpretation:
;     Represents a multiplication expression.

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define WRONG "something went wrong")

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; S-expr -> BSL-expr
; Produce a BSL-expr, if and only if the given S-expression s
; is the result of quoting a BSL expression that has a
; BSL-expr representative.
; Otherwise, throw an error.
(define (parse s)
  (cond
    [(atom? s) (parse-atom s)]
    [else (parse-sl s)]))

(check-expect (parse 3) 3)
(check-error (parse "three") WRONG)
(check-error (parse 'three) WRONG)
(check-expect (parse (list '* (list '+ 6 9) 0))
              (make-mul (make-add 6 9) 0))
 
; SL -> BSL-expr
; Produce a BSL-expr, if and only if the given SL s
; is the result of quoting a BSL expression that has a
; BSL-expr representative.
; Otherwise, throw an error.
(define (parse-sl s)
  (cond
    [(and (consists-of-3 s) (symbol? (first s)))
     (cond
       [(symbol=? (first s) '+)
        (make-add (parse (second s)) (parse (third s)))]
       [(symbol=? (first s) '*)
        (make-mul (parse (second s)) (parse (third s)))]
       [else (error WRONG)])]
    [else (error WRONG)]))

(check-error (parse-sl '()) WRONG)
(check-error (parse-sl (list 1 2 3)) WRONG)
(check-error (parse-sl (list '/ 3 4)) WRONG)
(check-expect (parse-sl (list '+ 3 4)) (make-add 3 4))
(check-expect (parse-sl (list '* 3 4)) (make-mul 3 4))
 
; Atom -> BSL-expr
; If s is a number, return s. Otherwise, throw
; an error.
(define (parse-atom s)
  (cond
    [(number? s) s]
    [(string? s) (error WRONG)]
    [(symbol? s) (error WRONG)]))

(check-expect (parse-atom 3) 3)
(check-error (parse-atom "three") WRONG)
(check-error (parse-atom 'three) WRONG)
 
; SL -> Boolean
; Does s have a length of 3?
(define (consists-of-3 s)
  (and (cons? s) (cons? (rest s)) (cons? (rest (rest s)))
       (empty? (rest (rest (rest s))))))

(check-expect (consists-of-3 '()) #false)
(check-expect (consists-of-3 (list 1 2)) #false)
(check-expect (consists-of-3 (list 1 2 3)) #true)
(check-expect (consists-of-3 (list 1 2 3 4)) #false)

; from ex316.rkt ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Any -> Boolean
; Is a of type atom?
(define (atom? a)
  (or (number? a)
      (string? a)
      (symbol? a)))
