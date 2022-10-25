;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex347) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

; A BSL-expr is one of:
; – Number
; – Add
; – Mul
; Interpretation:
;     Represents BSL expressions.

; A BSL-val is a Number.

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

(check-expect (eval-expression 3) 3)
(check-expect (eval-expression (make-add 1 1)) 2)
(check-expect (eval-expression (make-mul 3 10)) 30)
(check-expect (eval-expression (make-add (make-mul 1 1) 10)) 11)
(check-expect (eval-expression (make-add (make-mul 3.14
                                                   (make-mul 2 3))
                                         (make-mul 3.14
                                                   (make-mul -1 -9))))
              47.1)
