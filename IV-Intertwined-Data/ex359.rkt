;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex359) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A BSL-fun-expr is one of: 
; – Number
; – Symbol
; – FuncAppl
; – Add
; – Mul

; A BSL-var-expr is one of: 
; – Number
; – Symbol 
; – (make-add BSL-var-expr BSL-var-expr)
; – (make-mul BSL-var-expr BSL-var-expr)

(define-struct func-appl [name arg])
; A FuncAppl (short for function application) is a structure:
;     (make-func Symbol BSL-fun-expr)
; Interpretation:
;     The name and the argument of a function
;     Represents a function application.

(define-struct func-def [name para body])
; A BSL-fun-def (short for BSL function definition) is a structure:
;     (make-func Symbol Symbol BSL-fun-expr)
; Interpretation:
;     The name, parameter and body of a function.
;     Represents a function definition.

; A BSL-fun-def* is a [List-of BSL-fun-def].
;     Represents a definitions area that consists of
;     a number of one-argument function definitions.

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

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define F-FUNC (make-func-def 'f 'x (make-add 3 'x)))
(define G-FUNC (make-func-def 'g 'y (make-func-appl 'f (make-mul 2 'y))))
(define H-FUNC
  (make-func-def 'h 'v (make-add (make-func-appl 'f 'v)
                                 (make-func-appl 'g 'v))))

(define DA-FGH (list F-FUNC G-FUNC H-FUNC))

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define NO-FUNCTION "there is no such function")
(define WRONG "something went wrong")

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; BSL-fun-expr BSL-fun-def* -> Number
; Produce the result that DrRacket shows
; if you evaluate ex in the interactions area,
; assuming the definitions area da contains ex.
(define (eval-function* ex da)
  (local (; BSL-fun-expr -> Number
          (define (main ex1)
            (cond
              [(number? ex1) ex1]
              [(symbol? ex1) (error NO-FUNCTION)]
              [(func-appl? ex1) (func-eval* ex1)]
              [(add? ex1) (add-eval* ex1)]
              [(mul? ex1) (mul-eval* ex1)]))

          ; FuncAppl -> Number
          (define (func-eval* f)
            (local ((define VALUE (main (func-appl-arg f)))
                    (define FUNC-DEF (lookup-def da (func-appl-name f)))
                    (define PLUGD
                      (subst (func-def-body FUNC-DEF)
                             (func-def-para FUNC-DEF)
                             VALUE)))
              ; – IN –
              (eval-function* PLUGD da)))

          ; Add -> Number
          (define (add-eval* a)
            (+ (main (add-left a))
               (main (add-right a))))

          ; Mul -> Number
          (define (mul-eval* m)
            (* (main (mul-left m))
               (main (mul-right m)))))
    ; – IN –
    (main ex)))

(check-expect (eval-function* 6 DA-FGH)
              6)
(check-error (eval-function* 'x DA-FGH)
             NO-FUNCTION)
(check-expect (eval-function* (make-func-appl 'f 4) DA-FGH)
              7)
(check-expect (eval-function* (make-func-appl 'g 5) DA-FGH)
              13)
(check-expect (eval-function* (make-func-appl 'h 7) DA-FGH)
              27)
(check-expect (eval-function* (make-func-appl 'h (make-func-appl 'f 4)) DA-FGH)
              27)
(check-error (eval-function* (make-func-appl 'i 10) DA-FGH)
             NO-FUNCTION)
(check-expect (eval-function* (make-add 3 6) DA-FGH)
              9)
(check-expect (eval-function* (make-mul 6 3) DA-FGH)
              18)

; from ex358.rkt ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; BSL-fun-def* Symbol -> BSL-fun-def
; Retrieve the definition of f in da;
; signal an error if there is none.
(define (lookup-def da f)            
  (local ((define RES
            (filter (lambda (x) (symbol=? (func-def-name x) f)) da)))
    ; – IN –
    (cond
      [(empty? RES) (error NO-FUNCTION)]
      [else (first RES)])))

; from ex357.rkt ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; BSL-fun-expr Symbol Symbol BSL-fun-expr -> Number
; Determine the value of ex,
; i.e. determine the application
; of function f by replacing all
; occurrences of the parameter x
; in the function body b with the
; appropriate value.
(define (eval-definition1 ex f x b)
  (local (; BSL-fun-expr -> Number
          (define (main ex0)
            (cond
              [(number? ex0) ex0]
              [(symbol? ex0) (error WRONG)]
              [(func-appl? ex0) (eval-func-appl ex0)]
              [(add? ex0) (eval-add ex0)]
              [(mul? ex0) (eval-mul ex0)]))

          ; BSL-fun-expr -> Number
          (define (eval-func-appl ex1)
            (if (symbol=? (func-appl-name ex1) f)
                (local ((define value (eval-definition1 (func-appl-arg ex1)
                                                        f x b))
                        (define plugd (subst b x value)))
                  ; – IN –
                  (eval-definition1 plugd f x b))
                (error WRONG)))

          ; BSL-fun-expr -> Number
          (define (eval-add ex2)
            (+ (main (add-left ex2))
               (main (add-right ex2))))

          ; BSL-fun-expr -> Number
          (define (eval-mul ex3)
            (* (main (mul-left ex3))
               (main (mul-right ex3)))))
    ; – IN –
    (main ex)))

; from ex352.rkt ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; but deals with function applications as well ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

(check-expect (subst 4 'z 6)
              4)
(check-expect (subst 'x 'x 5)
              5)
(check-expect (subst (make-add 'x 3) 'x 6)
              (make-add 6 3))
(check-expect (subst (make-mul 1/2 (make-mul 'x 3)) 'x 7)
              (make-mul 1/2 (make-mul 7 3)))
(check-expect (subst (make-add (make-mul 'x 'x)
                               (make-mul 'y 'y))
                     'y 9)
              (make-add (make-mul 'x 'x)
                        (make-mul '9 '9)))
(check-expect (subst (make-func-appl 'g 5) 'y 5)
              (make-func-appl 'g 5))