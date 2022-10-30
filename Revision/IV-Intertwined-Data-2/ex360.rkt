;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex360) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; An Association is a list of two items:
;     (cons Symbol (cons Number '())).
; Interpretation:
;     Represents a constant definition, where the first
;     item is the name of the constant and the second
;     its value.

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
                             (make-mul 10 (make-func-appl 'area-of-cirle 'r))))

(define da (list CONSTANT FUNC1 FUNC2))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; BSL-da-all Symbol -> Association
; Produce the representation of the constant definition
; whose name is x, if such a piece of data exists in da.
; Throw an error otherwise.
(define (lookup-con-def da x)
  (cond
    [(empty? da)
     (error NO-SUCH-CONSTANT)]
    [else
     (local ((define FIRST (first da)))
       ; – IN –
       (if (and (cons? FIRST) (symbol=? x (first FIRST)))
           FIRST
           (lookup-con-def (rest da) x)))]))

(check-expect (lookup-con-def da 'close-to-pi)
              CONSTANT)
(check-error (lookup-con-def da 'close-to_pi)
             NO-SUCH-CONSTANT)

; BSL-da-all Symbol -> FuncDef
; Produce the representation of a function definition
; whose name is x, if such a piece of data exists in da.
; Throw an error otherwise.
(define (lookup-fun-def da f)
  (cond
    [(empty? da)
     (error NO-SUCH-FUNCTION)]
    [else
     (local ((define FIRST (first da)))
       ; – IN –
       (if (and (func-def? FIRST) (symbol=? f (func-def-name FIRST)))
           FIRST
           (lookup-fun-def (rest da) f)))]))

(check-expect (lookup-fun-def da 'area-of-circle)
              FUNC1)
(check-expect (lookup-fun-def da 'volume-of-10-cylinder)
              FUNC2)
(check-error (lookup-fun-def da 'no-such-function)
             NO-SUCH-FUNCTION)

; [X] [X -> Boolean] [X -> Symbol] String -> [BSL-da-all Symbol -> X]
; Return a function that produces the representation of X whose name is x,
; if such a piece of data exists in da. The function will Throw an error
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

(define const-lookup (lookup-abstract cons? first NO-SUCH-CONSTANT))

(check-expect (const-lookup da 'close-to-pi)
              (lookup-con-def da 'close-to-pi))
(check-error (const-lookup da 'close_to-pi)
             NO-SUCH-CONSTANT)

(define func-lookup (lookup-abstract func-def? func-def-name NO-SUCH-FUNCTION))

(check-expect (func-lookup da 'area-of-circle)
              (lookup-fun-def da 'area-of-circle))
(check-expect (func-lookup da 'volume-of-10-cylinder)
              (lookup-fun-def da 'volume-of-10-cylinder))
(check-error (func-lookup da 'nie-ma-takiej-funkcji)
             NO-SUCH-FUNCTION)

; [X] [X -> Boolean] [X -> Symbol] String -> [BSL-da-all Symbol -> X]
; Return a function that produces the representation of X whose name is x,
; if such a piece of data exists in da. The function will Throw an error
; if there is no such representation.
; The function rt? (right type) is used to find the right type of
; data in da; the function extract is used to extract the name of
; the data. The string err-str is an error string to be displayed
; when no representation can be produced.
(define (lookup-abstract.v2 rt? extract err-str)
  (local (; [X] BSL-da-all Symbol -> X
          ; Produce the representation of X whose name is x
          ; if such a piece of data exists in da.
          ; Throw an error otherwise.
          (define (main da x)
            (local ((define RESULT
                      (foldl
                       (lambda (da0 acc)
                         (if (and (rt? da0) (symbol=? x (extract da0)))
                             da0
                             acc))
                       #false
                       da)))
              ; – IN –
              (if (boolean? RESULT)
                  (error err-str)
                  RESULT))))
    ; – IN –
    main))

(define const-lookup.v2 (lookup-abstract.v2 cons? first NO-SUCH-CONSTANT))

(check-expect (const-lookup.v2 da 'close-to-pi)
              (lookup-con-def da 'close-to-pi))
(check-error (const-lookup.v2 da 'close_to-pi)
             NO-SUCH-CONSTANT)

(define func-lookup.v2
  (lookup-abstract.v2 func-def? func-def-name NO-SUCH-FUNCTION))

(check-expect (func-lookup.v2 da 'area-of-circle)
              (lookup-fun-def da 'area-of-circle))
(check-expect (func-lookup.v2 da 'volume-of-10-cylinder)
              (lookup-fun-def da 'volume-of-10-cylinder))
(check-error (func-lookup.v2 da 'nie-ma-takiej-funkcji)
             NO-SUCH-FUNCTION)