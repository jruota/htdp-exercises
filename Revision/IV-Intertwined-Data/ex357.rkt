;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex357) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A BSL-fun-expr is one of: 
; – Number
; – Symbol
; – FuncAppl
; – (make-add BSL-fun-expr BSL-var-expr)
; – (make-mul BSL-fun-expr BSL-var-expr)

; A BSL-var-expr is one of: 
; – Number
; – Symbol 
; – (make-add BSL-var-expr BSL-var-expr)
; – (make-mul BSL-var-expr BSL-var-expr)

(define-struct func-appl [name arg])
; A FuncAppl (short for function application) is a structure:
;     (make-func-appl Symbol BSL-fun-expr)
; Interpretation:
;     The name and the argument of a function.
;     Represents a function application.

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

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define WRONG "something went wrong")
(define FB (make-mul (make-add 'a 1) (make-add 'a 1)))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; BSL-fun-expr Symbol Symbol BSL-fun-expr -> Number
; Determine the value of ex,
; i.e. determine the application
; of function f by replacing all
; occurrences of the parameter x
; in the function body b with the
; appropriate value.
(define (eval-definition1 ex f x b)
  (local (; BSL-fun-expr -> Number
          ; Do the main work.
          (define (main ex0)
            (cond
              [(number? ex0) ex0]
              [(symbol? ex0)
               (error WRONG)]
              [(func-appl? ex0)
               (if (symbol=? f (func-appl-name ex0))
                   (local ((define value (main (func-appl-arg ex0)))
                           (define plugd (subst b x value)))
                     ; – IN –
                     (main plugd))
                   (error WRONG))]
              [(add? ex0)
               (+ (main (add-left ex0))
                  (main (add-right ex0)))]
              [(mul? ex0)
               (* (main (mul-left ex0))
                  (main (mul-right ex0)))])))
    ; – IN –
    (main ex)))

(check-error (eval-definition1 'sym 'func 'var (make-add 5 'var))
             WRONG)
(check-error (eval-definition1
              (make-func-appl 'function 6)
              'func 'var (make-add 5 'var))
             WRONG)

(check-expect (eval-definition1 5 'func 'var (make-add 5 'var))
              5)
(check-expect (eval-definition1 (make-add 4 7) 'func 'var (make-add 5 'var))
              11)
(check-expect (eval-definition1 (make-mul 2 3) 'func 'var (make-add 5 'var))
              6)

(check-expect (eval-definition1
               (make-func-appl 'k (make-add 1 1))
               'k 'a FB)
              9)
(check-expect (eval-definition1
               (make-mul 5 (make-func-appl 'k (make-add 1 1)))
               'k 'a FB)
              45)

(check-error (eval-definition1
              (make-mul (make-func-appl 'i 5)
                        (make-func-appl 'k (make-add 1 1)))
              'k 'a FB)
             WRONG)

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
