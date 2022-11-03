;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex359) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define-struct add [left right])
; An Add is a structure:
;     (make-add BSL-fun-expr BSL-fun-expr)
; Interpretation:
;     Represents an addition and
;     its two operands.

(define-struct mul [left right])
; A Mul is a structure:
;     (make-mul BSL-fun-expr BSL-fun-expr)
; Interpretation:
;     Represents a multiplication and
;     its two operands.

(define-struct func-appl [name arg])
; A FuncAppl (function application) is a structure:
;     (make-func-appl Symbol BSL-fun-expr)
; Interpretation:
;     Represents the application of a one-parameter function.
;     The "name" field is the name of the function
;     and the "arg" field is the function argument.

(define-struct func-def [name para body])
; A FuncDef (function definition) is a structure:
;     (make-func-def Symbol Symbol BSL-fun-def)
; Interpretation:
;     Represents the definition of a one-parameter function.
;     The "name" field is the name of the function,
;     the "para" field is the name of the function parameter
;     and the "body" field is the function body.

; A BSL-fun-expr is one of: 
; – Number
; – Symbol 
; – (make-add BSL-fun-expr BSL-fun-expr)
; – (make-mul BSL-fun-expr BSL-fun-expr)
; – (make-func-appl BSL-fun-expr BSL-fun-expr)

; A BSL-fun-def* is one of:
; – '()
; – (cons FuncDef BSL-fun-def*)
; Interpretation:
;     Represents a definitions area that consists
;     of a number of one-argument function definitions.

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define F (make-func-def 'f 'x (make-add 3 'x)))
(define G (make-func-def 'g 'y (make-func-appl 'f (make-mul 2 'y))))
(define H
  (make-func-def 'h
                 'v
                 (make-add (make-func-appl 'f 'v) (make-func-appl 'g 'v))))

(define da-fgh (list F G H))

(define FUNC-NOT-FOUND "there is no such function definition")
(define WRONG "something went wrong")
(define WRONG2 "unknown function")

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; BSL-fun-expr BSL-fun-def* -> Number
; Produce the result from evaluating ex, assuming
; its definition is in da.
(define (eval-function* ex da)
  (cond
    [(number? ex) ex]
    [(symbol? ex) (error WRONG)]
    [(add? ex)
     (+ (eval-function* (add-left ex) da)
        (eval-function* (add-right ex) da))]
    [(mul? ex)
     (* (eval-function* (mul-left ex) da)
        (eval-function* (mul-right ex) da))]
    [(func-appl? ex)
     (local ((define ARGUMENT (eval-function* (func-appl-arg ex) da))
             (define FUNC-DEF (lookup-def da (func-appl-name ex)))
             (define PLUGD
               (subst (func-def-body FUNC-DEF)
                      (func-def-para FUNC-DEF)
                      ARGUMENT)))
       ; – IN –
       (eval-function* PLUGD da))]))

(check-expect (eval-function* 3 da-fgh) 3)
(check-error (eval-function* 'symbol da-fgh) WRONG)
(check-expect (eval-function* (make-add 3 3) da-fgh) 6)
(check-expect (eval-function* (make-mul 4 5) da-fgh) 20)

(check-expect (eval-function* (make-func-appl 'f 7) da-fgh) 10)
(check-expect (eval-function* (make-func-appl 'g 3.5) da-fgh) 10)
(check-expect (eval-function* (make-func-appl 'h 1) da-fgh) 9)

(check-error (eval-function* (make-func-appl 'no-such-function 10) da-fgh)
             FUNC-NOT-FOUND)

(check-expect (eval-function*
               (make-func-appl              ; (+ 46 89) 
                'h
                (make-func-appl             ; 43
                 'g                     
                 (make-mul                  ; 20
                  2
                  (make-func-appl 'f 7))))  ; 10
               da-fgh)
              135)

; from ex358.rkt ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; BSL-fun-def* Symbol -> BSL-fun-def
; Retrieve the definition of function f in da,
; signal an error if there is none.
(define (lookup-def da f)
  (cond
    [(empty? da)
     (error FUNC-NOT-FOUND)]
    [else
     (local ((define FIRST (first da)))
       ; – IN –
       (if (symbol=? f (func-def-name FIRST))
           FIRST
           (lookup-def (rest da) f)))]))

; from ex357.rkt ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; BSL-fun-expr Symbol Number -> BSL-fun-expr
; Produce a BSL-fun-expr like ex with
; all occurrences of x replaced by v.
(define (subst ex x v)
  (cond
    [(number? ex) ex]
    [(symbol? ex)
     (if (symbol=? ex x) v ex)]
    [(add? ex)
     (make-add (subst (add-left ex) x v)
               (subst (add-right ex) x v))]
    [(mul? ex)
     (make-mul (subst (mul-left ex) x v)
               (subst (mul-right ex) x v))]
    [(func-appl? ex)
     (make-func-appl (func-appl-name ex)
                     (subst (func-appl-arg ex) x v))]))
  