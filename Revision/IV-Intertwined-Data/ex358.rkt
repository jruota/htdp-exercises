;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex358) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A BSL-fun-expr is one of: 
; – Number
; – Symbol
; – FuncAppl
; – Add
; – Mul

(define-struct func-appl [name arg])
; A FuncAppl (short for function application) is a structure:
;     (make-func Symbol BSL-fun-expr)
; Interpretation:
;     The name and the argument of a function
;     Represents a function application.

(define-struct func-def [name para body])
; A BSL-fun-def (short for BSL function definition) is a structure:
;     (make-func Symbol Symbol BSL-fun-expr)
; Interpretation:
;     The name, parameter and body of a function.
;     Represents a function definition.

; A BSL-fun-def* is a [List-of BSL-fun-def].
;     Represents a definitions area that consists of
;     a number of one-argument function definitions.

(define-struct add [left right])
; An Add is a structure:
;     (make-add BSL-fun-def BSL-fun-def)
; Interpretation:
;     Represents an addition and
;     its two operands.

(define-struct mul [left right])
; A Mul is a structure:
;     (make-mul BSL-fun-def BSL-fun-def)
; Interpretation:
;     Represents a multiplication and
;     its two operands.

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define F-FUNC (make-func-def 'f 'x (make-add 3 'x)))
(define G-FUNC (make-func-def 'g 'y (make-func-appl 'f (make-mul 2 'y))))
(define H-FUNC
  (make-func-def 'h 'v (make-add (make-func-appl 'f 'v)
                                 (make-func-appl 'g 'v))))

(define DA-FGH (list F-FUNC G-FUNC H-FUNC))

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define NO-FUNCTION "there is no such function")

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; BSL-fun-def* Symbol -> BSL-fun-def
; Retrieve the definition of f in da;
; signal an error if there is none.
(define (lookup-def da f)
  (local (; BSL-fun-def* -> BSL-fun-def
          ; Do the main work.
          (define (main da0)
            (cond
              [(empty? da0)
               (error NO-FUNCTION)]
              [else
               (local ((define func (first da0)))
                 ; – IN –
                 (if (symbol=? (func-def-name func) f)
                     func
                     (main (rest da0))))])))
    ; – IN –
    (main da)))

(check-error (lookup-def DA-FGH 'i) NO-FUNCTION)

(check-expect (lookup-def DA-FGH 'f) F-FUNC)
(check-expect (lookup-def DA-FGH 'g) G-FUNC)
(check-expect (lookup-def DA-FGH 'h) H-FUNC)
