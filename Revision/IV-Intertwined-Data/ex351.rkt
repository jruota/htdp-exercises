;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex351) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

; A BSL-val is one of:
; – Number
; – Boolean
; – String
; – Image

(define-struct add [left right])
; An Add is a structure:
;     (make-add BSL-expr BSL-expr)
; Interpretation:
;     Represents an addition and
;     its two operands.

(define-struct mul [left right])
; A Mul is a structure:
;     (make-mul BSL-expr BSL-expr)
; Interpretation:
;     Represents a multiplication and
;     its two operands.

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define WRONG "something went wrong")

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; S-expr -> Number
; Return the value of s if it
; represents a valid BSL-expr.
; Throw an error otherwise.
(define (interpreter-expr s)
  (eval-expression (parse s)))

(check-error (interpreter-expr "hello")
             WRONG)
(check-error (interpreter-expr '+)
             WRONG)
(check-error (interpreter-expr '(5))
             WRONG)
(check-error (interpreter-expr '(not #true))
             WRONG)
(check-error (interpreter-expr '(/ 4 5))
             WRONG)
(check-error (interpreter-expr '(+ 9 8 7 6 5 4 3 2 1 0))
             WRONG)
(check-error (interpreter-expr '(10 11 12))
             WRONG)

(check-expect (interpreter-expr '(+ 7 8))
              15)
(check-expect (interpreter-expr '(* 3 4))
              12)

(check-expect (interpreter-expr '(+ 3 (* 4 (+ 5 6))))
              47)

; from ex349.rkt ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; S-expr -> BSL-expr
(define (parse s)
  (cond
    [(atom? s) (parse-atom s)]
    [else (parse-sl s)]))

(check-expect (parse 4)
              4)
(check-error (parse "world")
             WRONG)
(check-error (parse 'y)
             WRONG)
 
; SL -> BSL-expr 
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

(check-error (parse-sl '(3))
             WRONG)
(check-error (parse-sl '(not #false))
             WRONG)
(check-error (parse-sl '(/ 5 6))
             WRONG)
(check-error (parse-sl '(+ 1 2 3 4 5))
             WRONG)
(check-error (parse-sl '(1 2 3 4 5))
             WRONG)

(check-expect (parse-sl '(+ 7 8))
              (make-add 7 8))
(check-expect (parse-sl '(* 3 4))
              (make-mul 3 4))

(check-expect (parse-sl '(+ 3 (* 4 (+ 5 6))))
              (make-add 3 (make-mul 4 (make-add 5 6))))
 
; Atom -> BSL-expr 
(define (parse-atom s)
  (cond
    [(number? s) s]
    [(string? s) (error WRONG)]
    [(symbol? s) (error WRONG)]))

(check-expect (parse-atom 3)
              3)
(check-error (parse-atom "hello")
             WRONG)
(check-error (parse-atom 'x)
             WRONG)

; SL -> Boolean
(define (consists-of-3 s)
  (and (cons? s) (cons? (rest s)) (cons? (rest (rest s)))
       (empty? (rest (rest (rest s))))))

; from ex347.rkt ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; BSL-expr -> BSL-val
; Compute the value of the representation
; of the BSL expression bsl.
(define (eval-expression bls)
  (cond
    [(number? bls) bls]
    [(add? bls)
     (+ (eval-expression (add-left bls))
        (eval-expression (add-right bls)))]
    [(mul? bls)
     (* (eval-expression (mul-left bls))
        (eval-expression (mul-right bls)))]))

; from ex316.rkt ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Any -> Boolean
; Is a of type atom?
(define (atom? a)
  (or (number? a)
      (string? a)
      (symbol? a)))
