;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex362) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; An S-expr is one of: 
; – Atom
; – SL

; An Atom is one of: 
; – Number
; – String
; – Symbol 

; An SL is one of: 
; – '()
; – (cons S-expr SL)

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
(define REPR-ERROR "illegal representation of an expression")

(define CONSTANT (list 'close-to-pi 3.14))
(define FUNC1 (make-func-def 'area-of-circle
                             'r
                             (make-mul 'close-to-pi (make-mul 'r 'r))))
(define FUNC2 (make-func-def 'volume-of-10-cylinder
                             'r
                             (make-mul 10 (make-func-appl 'area-of-circle 'r))))
(define F-FUNC (make-func-def 'f 'x (make-add 3 'x)))
(define G-FUNC (make-func-def 'g 'y (make-func-appl 'f (make-mul 2 'y))))
(define H-FUNC
  (make-func-def 'h 'v (make-add (make-func-appl 'f 'v)
                                 (make-func-appl 'g 'v))))

(define BSL-DA-ALL (list F-FUNC G-FUNC H-FUNC CONSTANT FUNC1 FUNC2))
(define SEXPR-DA-ALL
  (list
   '(define (f x) (+ 3 x))
   '(define (g y) (f (* 2 y)))
   '(define (h v) (+ (f v) (g v)))
   '(define close-to-pi 3.14)
   '(define (area-of-circle r) (* close-to-pi (* r r)))
   '(define (volume-of-10-cylinder r) (* 10 (area-of-circle r)))))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; S-expr SL -> Number
; Produce the result from parsing and evaluating ex, assuming
; its definition is in sl (which will be parsed as well.
(define (interpreter sexpr sl)
  (eval-all (parser sexpr) (map parser sl)))

(check-expect (interpreter 6 SEXPR-DA-ALL)
              6)
(check-error (interpreter 'x SEXPR-DA-ALL)
             NO-SUCH-CONSTANT)
(check-expect (interpreter '(f 4) SEXPR-DA-ALL)
              7)
(check-expect (interpreter '(g 5) SEXPR-DA-ALL)
              13)
(check-expect (interpreter '(h 7) SEXPR-DA-ALL)
              27)
(check-expect (interpreter '(h (f 4)) SEXPR-DA-ALL)
              27)
(check-error (interpreter '(i 10) SEXPR-DA-ALL)
             NO-SUCH-FUNCTION)
(check-expect (interpreter '(+ 3 6) SEXPR-DA-ALL)
              9)
(check-expect (interpreter '(* 6 3) SEXPR-DA-ALL)
              18)

(check-within (interpreter '(area-of-circle (+ 0 (* 1 1))) SEXPR-DA-ALL)
              3.14 0.1)
(check-within (interpreter '(volume-of-10-cylinder (* 1 (+ 3 -2))) SEXPR-DA-ALL)
              31.4 0.1)
(check-within (interpreter '(* 3 close-to-pi) SEXPR-DA-ALL)
              9.42 0.1)
(check-expect (interpreter 'close-to-pi SEXPR-DA-ALL)
              (second CONSTANT))

; S-expr -> AssocOrFuncDef
; Return the representation of sexpr in the form
; of a BSL-fun-expr.
(define (parser sexpr)
  (cond
    [(number? sexpr) sexpr]
    [(string? sexpr) (error REPR-ERROR)]
    [(symbol? sexpr) sexpr]
    ; function applications
    [(and (consists-of-2? sexpr) (symbol? (first sexpr)))
     (make-func-appl (first sexpr)
                     (parser (second sexpr)))]
    ; additions, multiplications, constant or function definitions
    [(and (consists-of-3? sexpr) (symbol? (first sexpr)))
     (local ((define FIRST (first sexpr)))
       ; – IN –
       (cond
         [(symbol=? FIRST '+)
          (make-add (parser (second sexpr))
                    (parser (third sexpr)))]
         [(symbol=? FIRST '*)
          (make-mul (parser (second sexpr))
                    (parser (third sexpr)))]
         [(symbol=? FIRST 'define)
          (local ((define SECOND (second sexpr))
                  (define THIRD (third sexpr)))
            ; – IN –
            (cond
              ; function definitions
              [(and (cons? SECOND) (consists-of-2? SECOND))
               (make-func-def (first SECOND)    ; function name
                              (second SECOND)   ; function parameter
                              (parser THIRD))]  ; function body
              ; constant definitions
              [(symbol? SECOND)
               (list SECOND (parser THIRD))]
              [else
               (error REPR-ERROR)]))]
         [else
          (error REPR-ERROR)]))]
    [else
     (error REPR-ERROR)]))

(check-expect (parser 42) 42)
(check-error (parser "string") REPR-ERROR)
(check-expect (parser 'symbol) 'symbol)
(check-expect (parser '(define close-to-pi 3.14))
              CONSTANT)
(check-expect (parser '(define (f x) (+ 3 x)))
              F-FUNC)
(check-expect (parser '(define (g y) (f (* 2 y))))
              G-FUNC)
(check-expect (parser '(define (h v) (+ (f v) (g v))))
              H-FUNC)
(check-expect (parser '(define (area-of-circle r) (* close-to-pi (* r r))))
              FUNC1)
(check-expect (parser
               '(define (volume-of-10-cylinder r) (* 10 (area-of-circle r))))
              FUNC2)

(check-error (parser (list 1 2 3))
             REPR-ERROR)
(check-error (parser (list 'symbol 2 3))
             REPR-ERROR)

(check-expect (parser '(define constant (+ 2 3)))
              (list 'constant (make-add 2 3)))
(check-error (parser '(define (f x y) (* 4 5)))
             REPR-ERROR)

; Any -> Boolean
; Does a have a length of 2?
(define (consists-of-2? a)
  (and (cons? a) (cons? (rest a)) (empty? (rest (rest a)))))

(check-expect (consists-of-2? '())
              #false)
(check-expect (consists-of-2? (list 1))
              #false)
(check-expect (consists-of-2? (list 1 2))
              #true)
(check-expect (consists-of-2? (list 1 2 3))
              #false)
(check-expect (consists-of-2? 12)
              #false)
(check-expect (consists-of-2? "ot")
              #false)
(check-expect (consists-of-2? 'two)
              #false)

; from ex361.rkt ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

; from ex349.rkt ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; SL -> Boolean
; Does s have a length of 3?
(define (consists-of-3? s)
  (and (cons? s) (cons? (rest s)) (cons? (rest (rest s)))
       (empty? (rest (rest (rest s))))))
