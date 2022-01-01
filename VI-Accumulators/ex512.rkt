;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex512) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

; Any -> Boolean
; Is any a Lam?
(define (is-lam? any)
  (or (is-var? any)
      (is-λ? any)
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
  (and (cons? lam)
       (= (length lam) 3)
       (symbol? (first lam))
       (symbol=? (first lam) 'λ)
       (cons? (second lam))
       (= (length (second lam)) 1)
       (symbol? (first (second lam)))
       (is-lam? (third lam))))

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

(check-expect (is-λ? ex1)
              #true)
(check-expect (is-λ? ex3)
              #true)
(check-expect (is-λ? ex4)
              #false)
(check-expect (is-λ? ex6)
              #false)
(check-expect (is-λ? (first ex6))
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
  (and (cons? lam)
       (= (length lam) 2)
       (or (is-λ? (first lam))
           (is-app? (first lam)))
       (is-lam? (second lam))))

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
      (first (second lam))
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
      (third lam)
      (error LAMBDA-ERROR)))

(check-expect (λ-body ex1)
              'x)
(check-expect (λ-body ex2)
              'y)
(check-expect (λ-body ex3)
              '(λ (x) y))
(check-error (λ-body ex4)
             LAMBDA-ERROR)

; Lam -> Lam
; Extract the function from
; a function application.
(define (app-fun lam)
  (if (is-app? lam)
      (first lam)
      (error LAMBDA-ERROR)))

(check-error (app-fun ex3)
             LAMBDA-ERROR)
(check-expect (app-fun ex4)
              '(λ (x) x))
(check-expect (app-fun ex5)
              '(λ (x) (x x)))
(check-expect (app-fun ex6)
              '((λ (y) (λ (x) y)) (λ (z) z)))

; Lam -> Lam
; Extract the argument from
; a function application.
(define (app-arg lam)
  (if (is-app? lam)
      (second lam)
      (error LAMBDA-ERROR)))

(check-error (app-arg ex3)
             LAMBDA-ERROR)
(check-expect (app-arg ex4)
              '(λ (x) x))
(check-expect (app-arg ex5)
              '(λ (x) (x x)))
(check-expect (app-arg ex6)
              '(λ (w) w))

; Lam -> [List-of Symbol]
; Produce the list of all symbols
; used as λ parameters in a λ term.
(define (declareds lam)
  (local (; Lam -> [List-of Symbol]
          ; Produce the list of all symbols
          ; used as λ parameters in a λ term.
          (define (declareds-helper lam0)
            (cond
              [(symbol? lam0)
               (if (symbol=? 'λ lam0)
                   '()
                   (list lam0))]
              [else
               (cond
                 [(empty? lam0)
                  '()]
                 [else
                  (append (declareds-helper (first lam0))
                          (declareds-helper (rest lam0)))])])))
    ; – IN –
    (if (is-lam? lam)
        (declareds-helper lam)
        (error LAMBDA-ERROR))))

(check-error (declareds "hello")
             LAMBDA-ERROR)

(check-expect (declareds 'f)
              (list 'f))
(check-expect (declareds (list 'h 'i))
              (list 'h 'i))

(check-expect (declareds ex1)
              (list 'x 'x))
(check-expect (declareds ex2)
              (list 'x 'y))
(check-expect (declareds ex3)
              (list 'y 'x 'y))
(check-expect (declareds ex4)
              (list 'x 'x 'x 'x))
(check-expect (declareds ex5)
              (list 'x 'x 'x 'x 'x 'x))
(check-expect (declareds ex6)
              (list 'y 'x 'y 'z 'z 'w 'w))
      