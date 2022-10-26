;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex355) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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
(define (eval-var-lookup ex da)
  (cond
    [(number? ex) ex]
    [(symbol? ex)
     (local ((define VALUE (assq ex da)))
       ; – IN –
       (if (cons? VALUE)
           (second VALUE)
           (error WRONG)))]
    [(add? ex)
     (+ (eval-var-lookup (add-left ex) da)
        (eval-var-lookup (add-right ex) da))]
    [(mul? ex)
     (* (eval-var-lookup (mul-left ex) da)
        (eval-var-lookup (mul-right ex) da))]))


(check-expect (eval-var-lookup
               (make-mul 3 (make-add 4 (make-mul 5 (make-add 6 7))))
               '())
              207)
(check-expect (eval-var-lookup
               (make-mul 3 (make-add 4 (make-mul 5 (make-add 6 7))))
               (list (list 'x 3) (list 'y 6) (list 'z 5) (list 'u 4)))
              207)
(check-expect (eval-var-lookup
               (make-mul 'x (make-add 'u (make-mul 'z (make-add 'y 7))))
               (list (list 'x 3) (list 'y 6) (list 'z 5) (list 'u 4)))
              207)
(check-error (eval-var-lookup
              (make-mul 'x (make-add 'w (make-mul 'z (make-add 'y 7))))
              (list (list 'x 3) (list 'y 6) (list 'z 5) (list 'u 4)))
             WRONG)
(check-error (eval-var-lookup
              (make-mul 'x (make-add 'u (make-mul 'z (make-add 'y 7))))
              (list (list 'x 3) (list 'y 6) (list 'z 5) (list 'w 4)))
             WRONG)
