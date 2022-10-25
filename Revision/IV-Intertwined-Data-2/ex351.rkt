;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex351) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; An S-expr is one of: 
; – Atom
; – SL
 
; An SL is one of: 
; – '()
; – (cons S-expr SL)

; An Atom is one of: 
; – Number
; – String
; – Symbol

; A BSL-expr is one of:
; – Number
; – Add
; – Mul
; Interpretation:
;     Represents BSL expressions.

(define-struct add [left right])
; An Add is a structure:
;     (make-add BSL-expr BSL-expr)
; Interpretation:
;     Represents an addition expression.

(define-struct mul [left right])
; A Mul is a structure:
;     (make-mul BSL-expr BSL-expr)
; Interpretation:
;     Represents a multiplication expression.

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define WRONG "something went wrong")

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; S-expr -> Number
; If s is a valid BSL-expr, return its value.
; Otherwise, throw an error.
(define (interpreter-expr s)
  (eval-expression (parse s)))

(check-expect (interpreter-expr 3) 3)
(check-error (interpreter-expr "string") WRONG)
(check-error (interpreter-expr 'symbol) WRONG)
(check-error (interpreter-expr (list 1 2)) WRONG)
(check-error (interpreter-expr (list 1 2 3)) WRONG)
(check-expect (interpreter-expr (list '+ 1 2)) 3)
(check-expect (interpreter-expr (list '* 4 8)) 32)
(check-expect (interpreter-expr (list '+ (list '* 3.14
                                               (list '* 2 3))
                                      (list '* 3.14
                                            (list '* -1 -9))))
              47.1)

; from ex350.rkt ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; S-expr -> BSL-expr
; Produce a BSL-expr, if and only if the given S-expression s
; is the result of quoting a BSL expression that has a
; BSL-expr representative.
; Otherwise, throw an error.
(define (parse s)
  (cond
    [(atom? s) (parse-atom s)]
    [else (parse-sl s)]))
 
; SL -> BSL-expr
; Produce a BSL-expr, if and only if the given SL s
; is the result of quoting a BSL expression that has a
; BSL-expr representative.
; Otherwise, throw an error.
(define (parse-sl s)
  (cond
    [(and (consists-of-3 s) (symbol? (first s)))
     (cond
       [(symbol=? (first s) '+)
        (make-add (parse (second s)) (parse (third s)))]
       [(symbol=? (first s) '*)
        (make-mul (parse (second s)) (parse (third s)))]
       [else (error WRONG)])]
    [else (error WRONG)]))
  
; Atom -> BSL-expr
; If s is a number, return s. Otherwise, throw
; an error.
(define (parse-atom s)
  (cond
    [(number? s) s]
    [(string? s) (error WRONG)]
    [(symbol? s) (error WRONG)]))
 
; SL -> Boolean
; Does s have a length of 3?
(define (consists-of-3 s)
  (and (cons? s) (cons? (rest s)) (cons? (rest (rest s)))
       (empty? (rest (rest (rest s))))))

; from ex347.rkt ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; BSL-expr -> BSL-val
; Compute the value of expr.
(define (eval-expression expr)
  (cond
    [(number? expr) expr]
    [(add? expr)
     (+ (eval-expression (add-left expr))
        (eval-expression (add-right expr)))]
    [(mul? expr)
     (* (eval-expression (mul-left expr))
        (eval-expression (mul-right expr)))]))
  
; from ex316.rkt ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Any -> Boolean
; Is a of type atom?
(define (atom? a)
  (or (number? a)
      (string? a)
      (symbol? a)))
