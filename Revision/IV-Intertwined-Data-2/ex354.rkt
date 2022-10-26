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

(define WRONG "argument is not a valid BSL-expr")

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
;  (local (; AL -> BSL-var-expr
;          ; Apply the associations in da0
;          ; to ex0.
;          (define (helper ex0 da0)
;            (cond
;              [(empty? da0) ex0]
;              [else
;               (helper (subst ex0 (first (first da0)) (second (first da0)))
;                       (rest da0))])))
;    ; – IN –
;    (eval-variable (helper ex da))))
  (cond
    [(empty? da)
     (eval-variable ex)]
    [else
     (local ((define FIRST (first da)))
       ; – IN –
       (eval-variable* (subst ex (first FIRST) (second FIRST))
                       (rest da)))]))

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
; If s is a valid BSL-expr, calculate its value.
; Throw an error otherwise.
(define (eval-variable s)
  (if (numeric? s)
      (eval-expression s)
      (error WRONG)))

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
; Produce a BSL-var-expr like ex with
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
               (subst (mul-right ex) x v))]))

; from ex347.rkt ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; BSL-expr -> BSL-val
; Compute the value of expr.
(define (eval-expression expr)
  (cond
    [(number? expr) expr]
    [(add? expr)
     (+ (eval-expression (add-left expr))
        (eval-expression (add-right expr)))]
    [(mul? expr)
     (* (eval-expression (mul-left expr))
        (eval-expression (mul-right expr)))]))
