;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex354) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A BSL-var-expr is one of: 
; – Number
; – Symbol 
; – (make-add BSL-var-expr BSL-var-expr)
; – (make-mul BSL-var-expr BSL-var-expr)

; A BSL-expr is one of:
; – Number
; – Add
; – Mul

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

; An AL (short for association list) is [List-of Association].

; An Association is a list of two items:
;   (cons Symbol (cons Number '())).

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define WRONG "argument is not a valid BSL-var-expr")

(define AL1 '())
(define AL2 (list (list 'x 47)))
(define AL3 (list (list 'x 47) (list 'y 33)))
(define AL4 (list (list 'x 47) (list 'y 33) (list 'z 158)))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; BSL-var-expr AL -> Number
; Starting from ex, apply subst to all
; associations in da. If numeric? holds
; for the result, determine its value;
; otherwise signal an error.
(define (eval-variable* ex da)
  (cond
    [(empty? da)
     (eval-variable ex)]
    [else
     (eval-variable*
      (subst ex (first (first da)) (second (first da)))
      (rest da))]))

(check-expect (eval-variable*
               (make-mul 3 (make-add 4 (make-mul 5 (make-add 6 7))))
               '())
              207)
(check-expect (eval-variable*
               (make-mul 3 (make-add 4 (make-mul 5 (make-add 6 7))))
               (list (list 'x 3) (list 'y 6) (list 'z 5) (list 'u 4)))
              207)
(check-expect (eval-variable*
               (make-mul 'x (make-add 'u (make-mul 'z (make-add 'y 7))))
               (list (list 'x 3) (list 'y 6) (list 'z 5) (list 'u 4)))
              207)
(check-error (eval-variable*
              (make-mul 'x (make-add 'w (make-mul 'z (make-add 'y 7))))
              (list (list 'x 3) (list 'y 6) (list 'z 5) (list 'u 4)))
             WRONG)
(check-error (eval-variable*
              (make-mul 'x (make-add 'u (make-mul 'z (make-add 'y 7))))
              (list (list 'x 3) (list 'y 6) (list 'z 5) (list 'w 4)))
             WRONG)

; BSL-var-expr -> Number
; If s satisfies the numeric? predicate,
; calculate its value. Throw an error otherwise.
(define (eval-variable s)
  (cond
    [(numeric? s)
     (eval-expression s)]
    [else
     (error WRONG)]))

(check-error (eval-variable 'x)
             WRONG)
(check-error (eval-variable (make-add 5 'y))
             WRONG)

(check-expect (eval-variable 254)
              254)
(check-expect (eval-variable (make-add
                              (make-mul
                               (make-add
                                (make-mul 1 2)
                                3)
                               4)
                              5))
              25)

; from ex353.rkt ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; BSL-var-expr -> Boolean
; Is bve a BSL-expr, as well?
(define (numeric? bve)
  (cond
    [(number? bve) #true]
    [(symbol? bve) #false]
    [(add? bve)
     (and (numeric? (add-left bve))
          (numeric? (add-right bve)))]
    [(mul? bve)
     (and (numeric? (mul-left bve))
          (numeric? (mul-right bve)))]))

; from ex352.rkt ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; BSL-var-expr Symbol Number -> BSL-var-expr
; Replace all occurrences of x in ex with v.
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
               (subst (mul-right ex) x v))]))

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
