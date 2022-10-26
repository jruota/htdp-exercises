;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex352) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

; A BSL-var-expr is one of: 
; – Number
; – Symbol 
; – (make-add BSL-var-expr BSL-var-expr)
; – (make-mul BSL-var-expr BSL-var-expr)

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

(check-expect (subst 333 'x 47)
              333)
(check-expect (subst 'x 'x 5) 5)
(check-expect (subst (make-add 'x 3) 'x 7)
              (make-add 7 3))
(check-expect (subst (make-mul 1/2 (make-mul 'x 3)) 'x 23)
              (make-mul 1/2 (make-mul 23 3)))
(check-expect (subst (make-mul 'x 'x) 'y 14.5)
              (make-mul 'x 'x))
(check-expect (subst (make-add (make-mul 'x 'x) (make-mul 'y 'y)) 'y 14.5)
              (make-add (make-mul 'x 'x) (make-mul 14.5 14.5)))
