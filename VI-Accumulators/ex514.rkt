;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex514) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A Lam is one of: 
; – a Symbol
; – (list 'λ (list Symbol) Lam)
; – (list Lam Lam)

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define ex1 '(λ (x) x))
(define ex2 '(λ (x) y))
(define ex3 '(λ (y) (λ (x) y)))
(define ex4 '((λ (x) x) (λ (x) x)))
(define ex5 '((λ (x) (x x)) (λ (x) (x x))))
(define ex6 '(((λ (y) (λ (x) y)) (λ (z) z)) (λ (w) w)))

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
                 (list 'λ (list para)
                   (undeclareds/a body newd)))]
              [(is-app? le)
               (local ((define fun (app-fun le))
                       (define arg (app-arg le)))
               (list (undeclareds/a fun declareds)
                     (undeclareds/a arg declareds)))])))
    (undeclareds/a le0 '())))

(check-expect (undeclareds ex1) ex1)
(check-expect (undeclareds ex2) '(λ (x) *undeclared))
(check-expect (undeclareds ex3) ex3)
(check-expect (undeclareds ex4) ex4)

; from ex512.rkt ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Any -> Boolean
; Is any a Lam?
(define (is-lam? any)
  (or (is-var? any)
      (is-λ? any)
      (and (cons? any)
           (= (length any) 2)
           (is-lam? (first any))
           (is-lam? (second any)))))

; Lam -> Boolean
; Is lam a variable?
(define (is-var? lam)
  (symbol? lam))

; Lam -> Boolean
; Is lam a lambda expression?
(define (is-λ? lam)
  (and (cons? lam)
       (= (length lam) 3)
       (symbol? (first lam))
       (symbol=? (first lam) 'λ)
       (cons? (second lam))
       (= (length (second lam)) 1)
       (symbol? (first (second lam)))
       (is-lam? (third lam))))

; Lam -> Boolean
; Is lam a variable?
(define (is-app? lam)
  (and (cons? lam)
       (= (length lam) 2)
       (or (is-λ? (first lam))
           (is-app? (first lam)))
       (is-lam? (second lam))))

; Lam -> Symbol
; Extract the parameter from a λ expression.
(define (λ-para lam)
  (if (is-λ? lam)
      (first (second lam))
      (error LAMBDA-ERROR)))

; Lam -> Lam
; Extract the body from a λ expression.
(define (λ-body lam)
  (if (is-λ? lam)
      (third lam)
      (error LAMBDA-ERROR)))

; Lam -> Lam
; Extract the function from
; a function application.
(define (app-fun lam)
  (if (is-app? lam)
      (first lam)
      (error LAMBDA-ERROR)))

; Lam -> Lam
; Extract the argument from
; a function application.
(define (app-arg lam)
  (if (is-app? lam)
      (second lam)
      (error LAMBDA-ERROR)))

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define free-and-bound '((λ (x) x) x))
(undeclareds free-and-bound)
