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

; A BSL-fun-expr is one of: 
; – Number
; – Symbol
; – FuncAppl
; – Add
; – Mul

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

(define-struct func-appl [name arg])
; A FuncAppl (short for function application) is a structure:
;     (make-func Symbol BSL-fun-expr)
; Interpretation:
;     The name and the argument of a function
;     Represents a function application.

; An Association is a list of two items:
;   (cons Symbol (cons Number '())).
; Interpretation:
;     Represents a single constant definition in the definitions area.

(define-struct func-def [name para body])
; A BSL-fun-def (short for BSL function definition) is a structure:
;     (make-func Symbol Symbol BSL-fun-expr)
; Interpretation:
;     The name, parameter and body of a function.
;     Represents a function definition.

; A DEF (short for definition) is one of:
; – Association
; – BSL-fun-def
; Interpretation:
;     A constant definitions ar a function definition.

; A BSL-da-all (Beginner Student Language definitions area all)
; is a [List-of DEF].

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define SEXPR-DA-ALL
  (list
   '(define (f x) (+ 3 x))
   '(define (g y) (f (* 2 y)))
   '(define (h v) (+ (f v) (g v)))
   '(define close-to-pi 3.14)
   '(define (area-of-circle r) (* close-to-pi (* r r)))
   '(define (volume-of-10-cylinder r) (* 10 (area-of-circle r)))))

(define CONS (list 'close-to-pi 3.14))
(define FUNC1 (make-func-def 'area-of-circle 'r
                             (make-mul 'close-to-pi (make-mul 'r 'r))))
(define FUNC2 (make-func-def 'volume-of-10-cylinder 'r
                             (make-mul 10 (make-func-appl 'area-of-circle 'r))))

(define F-FUNC (make-func-def 'f 'x (make-add 3 'x)))
(define G-FUNC (make-func-def 'g 'y (make-func-appl 'f (make-mul 2 'y))))
(define H-FUNC
  (make-func-def 'h 'v (make-add (make-func-appl 'f 'v)
                                 (make-func-appl 'g 'v))))

 (define BSL-DA-ALL (list F-FUNC G-FUNC H-FUNC CONS FUNC1 FUNC2))

(define CONS-ERROR "no such constant exists")
(define FUN-ERROR "no such function can be found")
(define REPR-ERROR "illegal representation of an expression")

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; S-expr SL -> Number
; Parse and then interpret the representation
; of an expression sexpr using the representations
; of constant and function definitions in sl.
(define (interpreter sexpr sl)
  (local ((define EXPRESSION (parser sexpr))
          (define DEFINITIONS (map parser sl)))
    ; – IN –
    (eval-all EXPRESSION DEFINITIONS)))

(check-expect (interpreter 6 SEXPR-DA-ALL)
              6)
(check-error (interpreter 'x SEXPR-DA-ALL)
             CONS-ERROR)
(check-expect (interpreter '(f 4) SEXPR-DA-ALL)
              7)
(check-expect (interpreter '(g 5) SEXPR-DA-ALL)
              13)
(check-expect (interpreter '(h 7) SEXPR-DA-ALL)
              27)
(check-expect (interpreter '(h (f 4)) SEXPR-DA-ALL)
              27)
(check-error (interpreter '(i 10) SEXPR-DA-ALL)
             FUN-ERROR)
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
              (second CONS))

; S-expr -> BSL-fun-expr
; Return the representation of an
; expression in the form of a BSL-fun-expr
; by parsin sexpr, itself a representation
; of an expression in the form of an S-expr.
(define (parser sexpr)
  (local (; S-expr -> BSL-fun-expr
          (define (main sexpr1)
            (cond
              [(atom? sexpr1) (parse-atom sexpr1)]
              [(cons? sexpr1) (parse-sl sexpr1)]))

          ; Atom -> BSL-fun-expr
          (define (parse-atom a)
            (cond
              [(number? a) a]
              [(string? a) (error REPR-ERROR)]
              [(symbol? a) a]))

          ; SL -> BSL-fun-expr
          (define (parse-sl s)
            (cond
              ;[(= (length s) 2)
              [(consists-of-2? s)
               (cond
                 [(symbol? (first s))
                  (make-func-appl (first s)
                                  (main (second s)))]
                 [else (error REPR-ERROR)])]
              ;[(= (length s) 3)
              [(consists-of-3? s)
               (cond
                 [(symbol=? (first s) '+)
                  (make-add (main (second s))
                            (main (third s)))]
                 [(symbol=? (first s) '*)
                  (make-mul (main (second s))
                            (main (third s)))]
                 [(and (symbol=? (first s) 'define)
                       (symbol? (second s))
                       (number? (third s)))
                  (list (second s) (third s))]
                 [(and (symbol=? (first s) 'define)
                       (cons? (second s))
                       (symbol? (first (second s)))
                       (symbol? (second (second s))))
                  (make-func-def (first (second s))
                                 (second (second s))
                                 (main (third s)))]
                 [else (error REPR-ERROR)])]
              [else (error REPR-ERROR)]))
          
          ; Any -> Boolean
          ; Is a of type atom?
          (define (atom? a)
            (or (number? a)
                (string? a)
                (symbol? a)))

          ; SL -> Boolean
          (define (consists-of-2? s)
            (and (cons? s) (cons? (rest s)) (empty? (rest (rest s)))))

          ; SL -> Boolean
          (define (consists-of-3? s)
            (and (cons? s) (cons? (rest s)) (cons? (rest (rest s)))
                 (empty? (rest (rest (rest s)))))))
    ; – IN –
    (main sexpr)))

(check-expect (parser 6)
              6)
(check-error (parser "hello world")
             REPR-ERROR)
(check-expect (parser 'x)
              'x)
(check-expect (parser '(+ 3 4))
              (make-add 3 4))
(check-expect (parser '(* 5 6))
              (make-mul 5 6))
(check-expect (parser '(* (+ 7 1) (* 9 8)))
              (make-mul (make-add 7 1) (make-mul 9 8)))
(check-expect (parser '(define close-to-pi 3.14))
              CONS)
(check-expect (parser '(f 2))
              (make-func-appl 'f 2))
; this does not make sense as a function application but should be caught
; in the evaluation step, not in the parsing step
(check-expect (parser '(x y))
              (make-func-appl 'x 'y))
(check-error (parser '(2 3))
             REPR-ERROR)
(check-expect (parser '(define (f x) (+ 3 x)))
              F-FUNC)
(check-expect (parser '(define (g y) (f (* 2 y))))
              G-FUNC)
(check-expect (parser '(define (h v) (+ (f v) (g v))))
              H-FUNC)
(check-error (parser '(x y z))
             REPR-ERROR)
(check-error (parser '(x y z u))
             REPR-ERROR)
; Should the parser handle this case as well?
; -> No, the parser from exercise 350 (or figure 125) suggests that the parser
;    should only handle "simple" representations of expressions that are limited
;    to the ones specified by the data definitions.
;(check-expect (parser SEXPR-DA-ALL)
;              BSL-DA-ALL)

; from ex361.rkt ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; BSL-fun-expr BSL-da-all -> Number
; Produce the result that DrRacket shows
; if you evaluate ex in the interactions area,
; assuming the definitions area da contains ex.
(define (eval-all ex da)
  (local (; BSL-fun-expr -> Number
          (define (main ex1)
            (cond
              [(number? ex1) ex1]
              [(symbol? ex1) (second (lookup-con-def da ex1))]
              [(func-appl? ex1) (eval-fun ex1)]
              [(add? ex1) (eval-add ex1)]
              [(mul? ex1) (eval-mul ex1)]))

          ; FuncAppl -> Number
          (define (eval-fun f)
            (local ((define FUN (lookup-fun-def da (func-appl-name f)))
                    (define VALUE (main (func-appl-arg f)))
                    (define PLUGD (subst (func-def-body FUN)
                                         (func-def-para FUN)
                                         VALUE)))
              ; – IN –
              (main PLUGD)))

          ; Add -> Number
          (define (eval-add a)
            (+ (main (add-left a))
               (main (add-right a))))

          ; Mul -> Number
          (define (eval-mul m)
            (* (main (mul-left m))
               (main (mul-right m)))))
    ; – IN –
    (main ex)))

; from ex360.rkt ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; BSL-da-all Symbol -> Association
; Return the representation of constant x
; if it exists in da.
(define (lookup-con-def da x)
  (local (; BSL-da-all -> Association
          (define (main da1)
            (cond
              [(empty? da1) (error CONS-ERROR)]
              [else
               (cond
                 [(func-def? (first da1)) (main (rest da1))]
                 [(cons? (first da1))
                  (if (right-constant? (first da1))
                      (first da1)
                      (main (rest da1)))])]))

          ; Association -> Boolean
          ; Does the association a represent
          ; the constant x?
          (define (right-constant? a)
            (symbol=? x (first a))))
    ; – IN –
    (main da)))

; BSL-da-all Symbol -> BSL-fun-def
; Return the representation of function f
; if it exists in da.
(define (lookup-fun-def da f)
  (local (; BSL-da-all -> BSL-fun-def
          (define (main da1)
            (cond
              [(empty? da1) (error FUN-ERROR)]
              [else
               (cond
                 [(func-def? (first da1))
                  (if (right-function? (first da1))
                      (first da1)
                      (main (rest da1)))]
                 [else (main (rest da1))])]))

          ; BSL-fun-def -> Boolean
          ; Does fd represent the function f?
          (define (right-function? fd)
            (symbol=? f (func-def-name fd))))
    ; – IN –
    (main da)))

; from ex359.rkt ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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