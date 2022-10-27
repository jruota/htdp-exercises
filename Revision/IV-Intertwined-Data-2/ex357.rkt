;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex357) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

(define-struct func-appl [name arg])
; A FuncAppl (function application) is a structure:
;     (make-func-appl Symbol BSL-fun-expr)
; Interpretation:
;     Represents the application of a one-parameter function.
;     The "name" field is the name of the function
;     and the "arg" field is the function argument.

; A BSL-fun-expr is one of: 
; – Number
; – Symbol 
; – (make-add BSL-fun-expr BSL-fun-expr)
; – (make-mul BSL-fun-expr BSL-fun-expr)
; – (make-func-appl BSL-fun-expr BSL-fun-expr)

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define WRONG "something went wrong")
(define WRONG2 "unknown function")
(define FB (make-mul (make-add 'a 1) (make-add 'a 1))) ; FB – function body
; this is for an infinite loop (function calls itself)
(define FB2 (make-mul
             (make-add (make-func-appl 'function 1) 1)
             'x))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; BSL-fun-expr Symbol Symbol BSL-fun-expr -> Number
; Determine the value of ex, i.e. determine the application
; of function f by replacing all occurrences of the function
; parameter x in the function body b with the
; appropriate value.
(define (eval-definition1 ex f x b)
  (cond
    [(number? ex) ex]
    [(symbol? ex) (error WRONG)]
    [(add? ex)
     (+ (eval-definition1 (add-left ex) f x b)
        (eval-definition1 (add-right ex) f x b))]
    [(mul? ex)
     (* (eval-definition1 (mul-left ex) f x b)
        (eval-definition1 (mul-right ex) f x b))]
    [(func-appl? ex)
     (if (symbol=? f (func-appl-name ex))
         (local ((define value ; function argument
                   (eval-definition1 (func-appl-arg ex) f x b))
                 ; function argument inside function body
                 (define plugd (subst b x value)))
           ; – IN –
           (eval-definition1 plugd f x b))
         (error WRONG2))]))

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
(check-expect (eval-definition1
               (make-mul (make-func-appl 'k 5)
                         (make-func-appl 'k (make-add 1 1)))
               'k 'a FB)
              324)
(check-expect (eval-definition1
               (make-mul 1 (make-func-appl 'k (make-func-appl 'k 1)))
               'k 'a FB)
              25)

(check-expect (eval-definition1
               (make-func-appl 'funcname
                               (make-func-appl 'funcname
                                               (make-func-appl 'funcname 4)))
               'funcname 'a FB)
              458329)

(check-error (eval-definition1 'x 'k 'y (make-add 'y 5))
             WRONG)
(check-error (eval-definition1
              (make-mul (make-func-appl 'i 5)
                        (make-func-appl 'k (make-add 1 1)))
              'k 'a FB)
             WRONG2)

; INFINTE LOOP !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;(check-expect (eval-definition1 (make-func-appl 'function 1) 'function 'x FB2)
;              1)

; from ex352.rkt (modified) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; BSL-fun-expr Symbol Number -> BSL-fun-expr
; Produce a BSL-fun-expr like ex with
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
               (subst (mul-right ex) x v))]
    [(func-appl? ex)
     (make-func-appl (func-appl-name ex)
                     (subst (func-appl-arg ex) x v))]))

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
(check-expect (subst (make-func-appl 'function (make-add 'x 5)) 'x -3)
              (make-func-appl 'function (make-add -3 5)))
