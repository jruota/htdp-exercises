;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex357) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A BSL-fun-expr is one of: 
; – Number
; – Symbol
; – Func
; – (make-add BSL-fun-expr BSL-var-expr)
; – (make-mul BSL-fun-expr BSL-var-expr)

(define-struct func [name arg])
; A Func (short for function) is a structure:
;     (make-func Symbol BSL-fun-expr)
; Interpretation:
;     The name and the argument of a function
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

; BSL-fun-expr Symbol Symbol BSL-fun-expr -> BSL-fun-expr
; Determine the value of ex,
; i.e. determine the application
; of function f by replacing all
; occurrences of the parameter x
; in the function body b with the
; appropriate value.
(define (eval-definition1 ex f x b)
  (cond
    [(number? ex) ex]
    [(symbol? ex) (error WRONG)]
    [(func? ex)
     (if (symbol=? (func-name ex) f)
         (local ((define value (eval-definition1 (func-arg ex) f x b))
                 (define plugd (subst b x value)))
           ; – IN –
           (eval-definition1 plugd f x b))
         (error WRONG))]
    [(add? ex)
     (+ (eval-definition1 (add-left ex) f x b)
        (eval-definition1 (add-right ex) f x b))]
    [(mul? ex)
     (* (eval-definition1 (mul-left ex) f x b)
        (eval-definition1 (mul-right ex) f x b))]))

(check-error (eval-definition1 'sym 'func 'var (make-add 5 'var))
             WRONG)
(check-error (eval-definition1
              (make-func 'function 6)
              'func 'var (make-add 5 'var))
             WRONG)

(check-expect (eval-definition1 5 'func 'var (make-add 5 'var))
              5)
(check-expect (eval-definition1 (make-add 4 7) 'func 'var (make-add 5 'var))
              11)
(check-expect (eval-definition1 (make-mul 2 3) 'func 'var (make-add 5 'var))
              6)

(check-expect (eval-definition1
               (make-func 'k (make-add 1 1))
               'k 'a FB)
              9)
(check-expect (eval-definition1
               (make-mul 5 (make-func 'k (make-add 1 1)))
               'k 'a FB)
              45)

(check-error (eval-definition1
              (make-mul (make-func 'i 5) (make-func 'k (make-add 1 1)))
              'k 'a FB)
             WRONG)

; from ex352.rkt ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; BSL-var-expr Symbol Number -> BSL-var-expr
; Replace all occurences of x in ex with v.
(define (subst ex x v)
  (cond
    [(number? ex) ex]
    [(symbol? ex)
     (if (symbol=? ex x)
         v
         ex)]
    [(add? ex)
     (make-add (subst (add-left ex) x v)
               (subst (add-right ex) x v))]
    [(mul? ex)
     (make-mul (subst (mul-left ex) x v)
               (subst (mul-right ex) x v))]))