;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex361) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

(define F-FUNC (make-func-def 'f 'x (make-add 3 'x)))
(define G-FUNC (make-func-def 'g 'y (make-func-appl 'f (make-mul 2 'y))))
(define H-FUNC
  (make-func-def 'h 'v (make-add (make-func-appl 'f 'v)
                                 (make-func-appl 'g 'v))))

(define BSL-DA-ALL (list F-FUNC G-FUNC H-FUNC CONS FUNC1 FUNC2))

(define CONS-ERROR "no such constant exists")
(define FUN-ERROR "no such function can be found")

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; BSL-fun-expr BSL-da-all -> Number
; Produce the result that DrRacket shows
; if you evaluate ex in the interactions area,
; assuming the definitions area da contains ex.
(define (eval-all ex da)
  (local (; BSL-fun-expr -> Number
          (define (main ex1)
            (cond
              [(number? ex1) ex1]
              [(symbol? ex1) (second (lookup-con-def da ex1))]
              [(func-appl? ex1) (eval-fun ex1)]
              [(add? ex1) (eval-add ex1)]
              [(mul? ex1) (eval-mul ex1)]))

          ; FuncAppl -> Number
          (define (eval-fun f)
            (local ((define FUN (lookup-fun-def da (func-appl-name f)))
                    (define VALUE (main (func-appl-arg f)))
                    (define PLUGD (subst (func-def-body FUN)
                                         (func-def-para FUN)
                                         VALUE)))
              ; – IN –
              (main PLUGD)))

          ; Add -> Number
          (define (eval-add a)
            (+ (main (add-left a))
               (main (add-right a))))

          ; Mul -> Number
          (define (eval-mul m)
            (* (main (mul-left m))
               (main (mul-right m)))))
    ; – IN –
    (main ex)))

(check-expect (eval-all 6 BSL-DA-ALL)
              6)
(check-error (eval-all 'x BSL-DA-ALL)
             CONS-ERROR)
(check-expect (eval-all (make-func-appl 'f 4) BSL-DA-ALL)
              7)
(check-expect (eval-all (make-func-appl 'g 5) BSL-DA-ALL)
              13)
(check-expect (eval-all (make-func-appl 'h 7) BSL-DA-ALL)
              27)
(check-expect (eval-all (make-func-appl 'h (make-func-appl 'f 4)) BSL-DA-ALL)
              27)
(check-error (eval-all (make-func-appl 'i 10) BSL-DA-ALL)
             FUN-ERROR)
(check-expect (eval-all (make-add 3 6) BSL-DA-ALL)
              9)
(check-expect (eval-all (make-mul 6 3) BSL-DA-ALL)
              18)

(check-within (eval-all (make-func-appl 'area-of-circle
                                        (make-add 0 (make-mul 1 1)))
                        BSL-DA-ALL)
              3.14 0.1)
(check-within (eval-all (make-func-appl 'volume-of-10-cylinder
                                        (make-mul 1 (make-add 3 -2)))
                        BSL-DA-ALL)
              31.4 0.1)
(check-within (eval-all (make-mul 3 'close-to-pi) BSL-DA-ALL)
              9.42 0.1)
(check-expect (eval-all 'close-to-pi BSL-DA-ALL)
              (second CONS))

; from ex360.rkt ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

; from ex359.rkt ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; BSL-fun-expr Symbol Number -> BSL-fun-expr
; Replace all occurrences of x in ex with v.
(define (subst ex x v)
  (local (; BSL-var-expr -> BSL-var-expr
          ; Replace all occurrences of x in ex0 with v.
          (define (main ex0)
            (cond
              [(number? ex0) (subst-number ex0)]
              [(symbol? ex0) (subst-symbol ex0)]
              [(func-appl? ex0) (subst-func ex0)]
              [(add? ex0) (subst-add ex0)]
              [(mul? ex0) (subst-mul ex0)]))

          ; Number -> Number
          ; Return n.
          (define (subst-number n)
            n)

          ; Symbol -> [Number or Symbol]
          ; Replace s with v if s is equal
          ; to x, otherwise return s.
          (define (subst-symbol s)
            (if (symbol=? s x)
                v
                s))

          ; FuncAppl -> FuncAppl
          ; Replace all occurrences of x in f with v.
          (define (subst-func f)
            (make-func-appl (func-appl-name f)
                            (subst (func-appl-arg f) x v)))

          ; Add -> Add
          ; Replace all occurrences of x in a with v.
          (define (subst-add a)
            (make-add (main (add-left a))
                      (main (add-right a))))

          ; Mul -> Mul
          ; Replace all occurrences of x in m with v.
          (define (subst-mul m)
            (make-mul (main (mul-left m))
                      (main (mul-right m)))))
    ; – IN –
    (main ex)))