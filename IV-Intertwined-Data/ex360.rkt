;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex360) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A BSL-fun-expr is one of: 
; – Number
; – Symbol
; – FuncAppl
; – Add
; – Mul

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

(define-struct func-appl [name arg])
; A FuncAppl (short for function application) is a structure:
;     (make-func Symbol BSL-fun-expr)
; Interpretation:
;     The name and the argument of a function
;     Represents a function application.

; An Association is a list of two items:
;   (cons Symbol (cons Number '())).
; Interpretation:
;     Represents a single constant definition in the definitions area.

(define-struct func-def [name para body])
; A BSL-fun-def (short for BSL function definition) is a structure:
;     (make-func Symbol Symbol BSL-fun-expr)
; Interpretation:
;     The name, parameter and body of a function.
;     Represents a function definition.

; A DEF (short for definition) is one of:
; – Association
; – BSL-fun-def
; Interpretation:
;     A constant definitions ar a function definition.

; A BSL-da-all (Beginner Student Language definitions area all)
; is a [List-of DEF].

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define CONS (list 'close-to-pi 3.14))
(define FUNC1 (make-func-def 'area-of-circle 'r
                             (make-mul 'close-to-pi (make-mul 'r 'r))))
(define FUNC2 (make-func-def 'volume-of-10-cylinder 'r
                             (make-mul 10 (make-func-appl 'area-of-circle 'r))))

(define BSL-DA-ALL (list CONS FUNC1 FUNC2))

(define CONS-ERROR "no such constant exists")
(define FUN-ERROR "no such function can be found")

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; BSL-da-all Symbol -> Association
; Return the representation of constant x
; if it exists in da.
(define (lookup-con-def da x)
  (local (; BSL-da-all -> Association
          (define (main da1)
            (cond
              [(empty? da1) (error CONS-ERROR)]
              [else
               (cond
                 [(func-def? (first da1)) (main (rest da1))]
                 [(cons? (first da1))
                  (if (right-constant? (first da1))
                      (first da1)
                      (main (rest da1)))])]))

          ; Association -> Boolean
          ; Does the association a represent
          ; the constant x?
          (define (right-constant? a)
            (symbol=? x (first a))))
    ; – IN –
    (main da)))

(check-error (lookup-con-def BSL-DA-ALL 'close-to-e)
             CONS-ERROR)
(check-expect (lookup-con-def BSL-DA-ALL 'close-to-pi)
              CONS)

; BSL-da-all Symbol -> BSL-fun-def
; Return the representation of function f
; if it exists in da.
(define (lookup-fun-def da f)
  (local (; BSL-da-all -> BSL-fun-def
          (define (main da1)
            (cond
              [(empty? da1) (error FUN-ERROR)]
              [else
               (cond
                 [(func-def? (first da1))
                  (if (right-function? (first da1))
                      (first da1)
                      (main (rest da1)))]
                 [else (main (rest da1))])]))

          ; BSL-fun-def -> Boolean
          ; Does fd represent the function f?
          (define (right-function? fd)
            (symbol=? f (func-def-name fd))))
    ; – IN –
    (main da)))

(check-error (lookup-fun-def BSL-DA-ALL 'non-existing-function)
             FUN-ERROR)
(check-expect (lookup-fun-def BSL-DA-ALL 'area-of-circle)
              FUNC1)
(check-expect (lookup-fun-def BSL-DA-ALL 'volume-of-10-cylinder)
              FUNC2)