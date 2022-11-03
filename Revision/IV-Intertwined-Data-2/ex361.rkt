;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex361) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

(define-struct func-def [name para body])
; A FuncDef (function definition) is a structure:
;     (make-func-def Symbol Symbol BSL-fun-expr)
; Interpretation:
;     Represents the definition of a one-parameter function.
;     The "name" field is the name of the function,
;     the "para" field is the name of the function parameter
;     and the "body" field is the function body.

; A BSL-fun-expr is one of: 
; – Number
; – Symbol 
; – (make-add BSL-fun-expr BSL-fun-expr)
; – (make-mul BSL-fun-expr BSL-fun-expr)
; – (make-func-appl Symbol BSL-fun-expr)

; An Association is a list of two items:
;     (cons Symbol (cons Number '())).
; Interpretation:
;     Represents a constant definition, where the first
;     item is the name of the constant and the second
;     its value.

; An AssocOrFuncDef is one of:
; – Association
; – FuncDef

; A BSL-da-all (Beginning Student Language definitions area)
; is one of
; – '()
; – (cons AssocOrFuncDef BSL-da-all)
; Interpretation:
;     A collection of constant and function definitions.

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define NO-SUCH-CONSTANT "there is no such constant")
(define NO-SUCH-FUNCTION "there is no such function")

(define CONSTANT (list 'close-to-pi 3.14))
(define FUNC1 (make-func-def 'area-of-circle
                             'r
                             (make-mul 'close-to-pi (make-mul 'r 'r))))
(define FUNC2 (make-func-def 'volume-of-10-cylinder
                             'r
                             (make-mul 10 (make-func-appl 'area-of-circle 'r))))

(define da (list CONSTANT FUNC1 FUNC2))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; BSL-fun-expr BSL-da-all -> Number
; Produce the result from evaluating ex, assuming
; its definition is in da.
(define (eval-all ex da)
  (cond
    [(number? ex) ex]
    [(symbol? ex)
     (local ((define
               const-lookup (lookup-abstract cons? first NO-SUCH-CONSTANT)))
       ; – IN –
       (second (const-lookup da ex)))]
    [(add? ex)
     (+ (eval-all (add-left ex) da)
        (eval-all (add-right ex) da))]
    [(mul? ex)
     (* (eval-all (mul-left ex) da)
        (eval-all (mul-right ex) da))]
    [(func-appl? ex)
     (local ((define
               func-lookup (lookup-abstract func-def?
                                            func-def-name
                                            NO-SUCH-FUNCTION))
             (define function (func-lookup da (func-appl-name ex)))
             (define value (eval-all (func-appl-arg ex) da))
             (define plugd (subst (func-def-body function)
                                  (func-def-para function)
                                  value)))
       ; – IN –
       (eval-all plugd da))]))

(check-expect (eval-all 3 da) 3)
(check-expect (eval-all 'close-to-pi da) 3.14)
(check-error (eval-all 'nope da) NO-SUCH-CONSTANT)
(check-expect (eval-all (make-add 3 4) da) 7)
(check-expect (eval-all (make-add 2.86 'close-to-pi) da) 6)
(check-expect (eval-all (make-mul 7 6) da) 42)
(check-expect (eval-all (make-mul 5
                                  (make-func-appl
                                   'area-of-circle
                                   (make-add .5 .5)))
                        da)
              (* 3.14 5))
(check-expect (eval-all (make-func-appl 'area-of-circle 1) da)
              3.14)
(check-expect (eval-all (make-func-appl 'volume-of-10-cylinder
                                        (make-add .354 .646))
                        da)
              (* 3.14 10))
(check-error (eval-all (make-func-appl 'no-such-function 67) da)
             NO-SUCH-FUNCTION)

; from ex360.rkt ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; [X] [X -> Boolean] [X -> Symbol] String -> [BSL-da-all Symbol -> X]
; Return a function that produces the representation of X whose name is x,
; if such a piece of data exists in da. The function will throw an error
; if there is no such representation.
; The function rt? (right type) is used to find the right type of
; data in da; the function extract is used to extract the name of
; the data. The string err-str is an error string to be displayed
; when no representation can be produced.
(define (lookup-abstract rt? extract err-str)
  (local (; [X] BSL-da-all Symbol -> X
          ; Produce the representation of X whose name is x
          ; if such a piece of data exists in da.
          ; Throw an error otherwise.
          (define (main da x)
            (cond
              [(empty? da)
               (error err-str)]
              [else
               (local ((define FIRST (first da)))
                 ; – IN –
                 (if (and (rt? FIRST) (symbol=? x (extract FIRST)))
                     FIRST
                     (main (rest da) x)))])))
    ; – IN –
    main))

; from ex357.rkt ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
