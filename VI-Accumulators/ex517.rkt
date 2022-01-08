;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex517) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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
; Replace all occurrences of variables
; with a natural number that represents
; how far away the declaring λ is.
(define (static-distance lam)
  (local (; Lam [List-of Symbol] -> Lam
          ; Replace all occurrences of variables
          ; with a natural number that represents
          ; how far away the declaring λ is.
          ; The accumulator lop is a list of all λ 
          ; parameters on the path from lam0 to lam.
          (define (static-distance/a lam0 lop)
            (cond
              [(is-var? lam0)
               (if (member? lam0 lop)
                   (first (indexes-of lop lam0))
                   lam0)]
              [(is-λ? lam0)
               (local ((define parameter (λ-para lam0)))
                 ; – IN –
                 (list 'λ (list parameter)
                       (static-distance/a (λ-body lam0)
                                          (cons parameter lop))))]
              [(is-app? lam0)
               (list (static-distance/a (app-fun lam0) lop)
                     (static-distance/a (app-arg lam0) lop))]
              [(is-lam? lam0) ; must be (list Lam Lam)
               (list (static-distance/a (first lam0) lop)
                     (static-distance/a (second lam0) lop))]
              [else
               (error LAMBDA-ERROR)])))
    ; – IN –
    (static-distance/a lam '())))

(check-error (static-distance "hello")
             LAMBDA-ERROR)
(check-expect (static-distance 'a)
              'a)
(check-expect (static-distance ex1)
              (list 'λ (list 'x) 0))
(check-expect (static-distance ex3)
              (list 'λ (list 'y) (list 'λ (list 'x) 1)))
(check-expect (static-distance '((λ (x) ((λ (y) (y x)) x)) (λ (z) z)))
              (list (list 'λ (list 'x) (list (list 'λ (list 'y) (list 0 1)) 0))
                    (list 'λ (list 'z) 0)))

; [List-of X] X -> [List-of Number]
; Return a list of all the indexes
; where val occurs in lst. 
(define (indexes-of lst val)
  (local (; [List-of X] N -> [List-of Number]
          ; Helper function.
          (define (main lst0 n)
            (cond
              [(empty? lst0) '()]
              [else
               (if (equal? (first lst0) val)
                   (cons n (main (rest lst0) (add1 n)))
                   (main (rest lst0) (add1 n)))])))
    ; – IN –
    (main lst 0)))

(check-expect (indexes-of (list "hello" "world" "how" "are" "you") 42)
              '())
(check-expect (indexes-of (list 1 2 3 4 5) 4)
              (list 3))
(check-expect (indexes-of (list 'a 'f 'b 'f 'c 'f 'd) 'f)
              (list 1 3 5))

; from ex515.rkt ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
               (if (member? le declareds)
                   (list '*declared le)
                   (list '*undeclared le))]
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
