;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex353) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; BSL-var-expr -> Boolean
; Is bve0 a BSL-expr, as well?
(define (numeric? bve0)
  (local (; BSL-var-expr -> Boolean
          ; Is bve1 a BSL-expr, as well?
          (define (main? bve1)
            (cond
              [(number? bve1) #true]
              [(symbol? bve1) #false]
              [(add? bve1) (add-numeric? bve1)]
              [(mul? bve1) (mul-numeric? bve1)]))

          ; Add -> Boolean
          ; Is a a BSL-expr?
          (define (add-numeric? a)
            (and (main? (add-left a))
                 (main? (add-right a))))

          ; Mul -> Boolean
          ; Is m a BSL-expr?
          (define (mul-numeric? m)
            (and (main? (mul-left m))
                 (main? (mul-right m)))))
    ; – IN –
    (main? bve0)))

(check-expect (numeric? 5)
              #true)
(check-expect (numeric? 'x)
              #false)
(check-expect (numeric? (make-add 3 4))
              #true)
(check-expect (numeric? (make-add 5 'y))
              #false)
(check-expect (numeric? (make-mul 8 9))
              #true)
(check-expect (numeric? (make-mul 'z 2))
              #false)
(check-expect (numeric?
               (make-mul
                (make-add
                 1
                 (make-mul
                  99
                  (make-mul 6 'r)))
                77))
              #false)
(check-expect (numeric?
               (make-mul
                (make-add
                 1
                 (make-mul
                  99
                  (make-mul 6 3)))
                77))
              #true)