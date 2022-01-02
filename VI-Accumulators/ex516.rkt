;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex516) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A Lam is one of: 
; – a Symbol
; – (list Lam Lam)
; – (make-λ-fun Symbol Lam)
; – (make-λ-app make-λ-fun Lam)

(define-struct λ-fun [para body])
; A λ-fun-def (short for lambda function definition) is a structure:
;     (make-λ-fun Symbol Lam)
; Interpretation:
;     The parameter and body of a function.
;     Represents a function definition.

(define-struct λ-app [fun arg])
; A λ-fun-app (short lambda function application) is a structure:
;     (make-λ-app λ-fun-def Lam)
; Interpretation:
;     The function definition and the argument of a function application.
;     Represents a function application.

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define ex1 (make-λ-fun 'x 'x))
(define ex2 (make-λ-fun 'x 'y))
(define ex3 (make-λ-fun 'y (make-λ-fun 'x 'y)))
(define ex4 (make-λ-app (make-λ-fun 'x 'x) (make-λ-fun 'x 'x)))
(define ex5 (make-λ-app (make-λ-fun 'x '(x x)) (make-λ-fun 'x '(x x))))
(define ex6 (make-λ-app
             (make-λ-app (make-λ-fun 'y (make-λ-fun 'x 'y)) (make-λ-fun 'z 'z))
             (make-λ-fun 'w 'w)))

(define LAMBDA-ERROR "expected a λ expression")

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Lam -> Lam 
; replaces all symbols s in le with '*undeclared
; if they do not occur within the body of a λ 
; expression whose parameter is s
(define (undeclareds le0)
  (local (; Lam [List-of Symbol] -> Lam
          ; accumulator declareds is a list of all λ 
          ; parameters on the path from le0 to le
          (define (undeclareds/a le declareds)
            (cond
              [(is-var? le)
               (if (member? le declareds) le '*undeclared)]
              [(is-λ? le)
               (local ((define para (λ-para le))
                       (define body (λ-body le))
                       (define newd (cons para declareds)))
                 (make-λ-fun para
                             (undeclareds/a body newd)))]
              [(is-app? le)
               (local ((define fun (app-fun le))
                       (define arg (app-arg le)))
               (make-λ-app (undeclareds/a fun declareds)
                           (undeclareds/a arg declareds)))])))
    (undeclareds/a le0 '())))

(check-expect (undeclareds ex1) ex1)
(check-expect (undeclareds ex2) (make-λ-fun 'x '*undeclared))
(check-expect (undeclareds ex3) ex3)
(check-expect (undeclareds ex4) ex4)

; Any -> Boolean
; Is any a Lam?
(define (is-lam? any)
  (or (is-var? any)
      (is-λ? any)
      (is-app? any)
      (and (cons? any)
           (= (length any) 2)
           (is-lam? (first any))
           (is-lam? (second any)))))

(check-expect (is-lam? 99)
              #false)
(check-expect (is-lam? "hello")
              #false)
(check-expect (is-lam? (make-posn 1 2))
              #false)
(check-expect (is-lam? empty-image)
              #false)
(check-expect (is-lam? (lambda (x) x))
              #false)

(check-expect (is-lam? '(x x))
              #true)

(check-expect (is-lam? ex1)
              #true)
(check-expect (is-lam? ex2)
              #true)
(check-expect (is-lam? ex3)
              #true)
(check-expect (is-lam? ex4)
              #true)
(check-expect (is-lam? ex5)
              #true)
(check-expect (is-lam? ex6)
              #true)

; Lam -> Boolean
; Is lam a variable?
(define (is-var? lam)
  (symbol? lam))

(check-expect (is-var? 99)
              #false)
(check-expect (is-var? "hello")
              #false)
(check-expect (is-var? (make-posn 1 2))
              #false)
(check-expect (is-var? empty-image)
              #false)

(check-expect (is-var? ex1)
              #false)
(check-expect (is-var? ex6)
              #false)

(check-expect (is-var? 'a)
              #true)
(check-expect (is-var? 'λ)
              #true)

; Lam -> Boolean
; Is lam a lambda expression?
(define (is-λ? lam)
  (and (λ-fun? lam)
       (symbol? (λ-fun-para lam))
       (or (symbol? (λ-fun-body lam))
           (and (cons? (λ-fun-body lam))
                (= (length (λ-fun-body lam)) 2)
                (is-lam? (first (λ-fun-body lam)))
                (is-lam? (second (λ-fun-body lam))))
           (is-λ? (λ-fun-body lam))
           (is-app? (λ-fun-body lam)))))

(check-expect (is-λ? 99)
              #false)
(check-expect (is-λ? "hello")
              #false)
(check-expect (is-λ? (make-posn 1 2))
              #false)
(check-expect (is-λ? empty-image)
              #false)
(check-expect (is-λ? (lambda (x) x))
              #false)

(check-expect (is-λ? (make-λ-fun 'x '(x x)))
              #true)

(check-expect (is-λ? ex1)
              #true)
(check-expect (is-λ? ex3)
              #true)
(check-expect (is-λ? ex4)
              #false)
(check-expect (is-λ? ex6)
              #false)
(check-expect (is-λ? (λ-app-fun ex6))
              #false)

(check-expect (is-λ? 'a)
              #false)
(check-expect (is-λ? 'λ)
              #false)

(check-expect (is-λ? (list "hello" (list 'a) 'a))
              #false)
(check-expect (is-λ? (list 'λ (list 'a 'b) (list 'a 'b)))
              #false)

; Lam -> Boolean
; Is lam a variable?
(define (is-app? lam)
  (and (λ-app? lam)
       #true))

(check-expect (is-app? 99)
              #false)
(check-expect (is-app? "hello")
              #false)
(check-expect (is-app? (make-posn 1 2))
              #false)
(check-expect (is-app? empty-image)
              #false)
(check-expect (is-app? 'a)
              #false)
(check-expect (is-app? 'λ)
              #false)

(check-expect (is-app? ex1)
              #false)
(check-expect (is-app? ex3)
              #false)
(check-expect (is-app? ex4)
              #true)
(check-expect (is-app? ex5)
              #true)
(check-expect (is-app? ex6)
              #true)

(check-expect (is-app? (list 'a '(λ (x) x)))
              #false)

; Lam -> Symbol
; Extract the parameter from a λ expression.
(define (λ-para lam)
  (if (is-λ? lam)
      (λ-fun-para lam)
      (error LAMBDA-ERROR)))

(check-expect (λ-para ex1)
              'x)
(check-expect (λ-para ex2)
              'x)
(check-expect (λ-para ex3)
              'y)
(check-error (λ-para ex4)
             LAMBDA-ERROR)

; Lam -> Lam
; Extract the body from a λ expression.
(define (λ-body lam)
  (if (is-λ? lam)
      (λ-fun-body lam)
      (error LAMBDA-ERROR)))

(check-expect (λ-body ex1)
              'x)
(check-expect (λ-body ex2)
              'y)
(check-expect (λ-body ex3)
              (make-λ-fun 'x 'y))
(check-error (λ-body ex4)
             LAMBDA-ERROR)

; Lam -> Lam
; Extract the function from
; a function application.
(define (app-fun lam)
  (if (is-app? lam)
      (λ-app-fun lam)
      (error LAMBDA-ERROR)))

(check-error (app-fun ex3)
             LAMBDA-ERROR)
(check-expect (app-fun ex4)
              (make-λ-fun 'x 'x))
(check-expect (app-fun ex5)
              (make-λ-fun 'x '(x x)))
(check-expect (app-fun ex6)
              (make-λ-app
               (make-λ-fun 'y (make-λ-fun 'x 'y))
               (make-λ-fun 'z 'z)))
; Lam -> Lam
; Extract the argument from
; a function application.
(define (app-arg lam)
  (if (is-app? lam)
      (λ-app-arg lam)
      (error LAMBDA-ERROR)))

(check-error (app-arg ex3)
             LAMBDA-ERROR)
(check-expect (app-arg ex4)
              (make-λ-fun 'x 'x))
(check-expect (app-arg ex5)
              (make-λ-fun 'x '(x x)))
(check-expect (app-arg ex6)
              (make-λ-fun 'w 'w))
