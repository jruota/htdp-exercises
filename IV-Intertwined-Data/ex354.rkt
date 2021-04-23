;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex354) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A BSL-var-expr is one of: 
; – Number
; – Symbol 
; – (make-add BSL-var-expr BSL-var-expr)
; – (make-mul BSL-var-expr BSL-var-expr)

; A BSL-expr is one of:
; – Number
; – Add
; – Mul

(define-struct add [left right])
; An Add is a structure:
;     (make-add BSL-expr BSL-expr)
; Interpretation:
;     Represents an addition and
;     its two operands.

(define-struct mul [left right])
; A Mul is a structure:
;     (make-mul BSL-expr BSL-expr)
; Interpretation:
;     Represents a multiplication and
;     its two operands.

; An AL (short for association list) is [List-of Association].

; An Association is a list of two items:
;   (cons Symbol (cons Number '())).

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define WRONG "argument is not a valid BSL-var-expr")

(define AL1 '())
(define AL2 (list (list 'x 47)))
(define AL3 (list (list 'x 47) (list 'y 33)))
(define AL4 (list (list 'x 47) (list 'y 33) (list 'z 158)))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; BSL-var-expr AL -> Number
; Starting from ex, apply subst to all
; associations in da. If numeric? holds
; for the result, determine its value;
; otherwise signal an error.
(define (eval-variable* ex da)
  (cond
    [(empty? da) (eval-variable ex)]
    [else
     (eval-variable*
      (subs ex (first (first da)) (second (first da)))
      (rest da))]))

(check-expect (eval-variable*
               (make-mul 3 (make-add 4 (make-mul 5 (make-add 6 7))))
               '())
              207)
(check-expect (eval-variable*
               (make-mul 3 (make-add 4 (make-mul 5 (make-add 6 7))))
               (list (list 'x 3) (list 'y 6) (list 'z 5) (list 'u 4)))
              207)
(check-expect (eval-variable*
               (make-mul 'x (make-add 'u (make-mul 'z (make-add 'y 7))))
               (list (list 'x 3) (list 'y 6) (list 'z 5) (list 'u 4)))
              207)
(check-error (eval-variable*
              (make-mul 'x (make-add 'w (make-mul 'z (make-add 'y 7))))
              (list (list 'x 3) (list 'y 6) (list 'z 5) (list 'u 4)))
             WRONG)
(check-error (eval-variable*
              (make-mul 'x (make-add 'u (make-mul 'z (make-add 'y 7))))
              (list (list 'x 3) (list 'y 6) (list 'z 5) (list 'w 4)))
             WRONG)

; BSL-var-expr -> Number
; If s satisfies the numeric? predicate,
; calculate its value. Throw an error otherwise.
(define (eval-variable s)
  (if (numeric? s)
      (eval-expression s)
      (error WRONG)))

(check-error (eval-variable 'x)
             WRONG)
(check-error (eval-variable (make-add 5 'y))
             WRONG)

(check-expect (eval-variable 254)
              254)
(check-expect (eval-variable (make-add
                              (make-mul
                               (make-add
                                (make-mul 1 2)
                                3)
                               4)
                              5))
              25)

; from ex353.rkt ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; BSL-var-expr -> Boolean
; Is bve0 a BSL-expr, as well?
(define (numeric? bve0)
  (local (; BSL-var-expr -> Boolean
          ; Is bve1 a BSL-expr, as well?
          (define (main? bve1)
            (cond
              [(number? bve1) #true]
              [(symbol? bve1) #false]
              [(add? bve1) (add-numeric? bve1)]
              [(mul? bve1) (mul-numeric? bve1)]))

          ; Add -> Boolean
          ; Is a a BSL-expr?
          (define (add-numeric? a)
            (and (main? (add-left a))
                 (main? (add-right a))))

          ; Mul -> Boolean
          ; Is m a BSL-expr?
          (define (mul-numeric? m)
            (and (main? (mul-left m))
                 (main? (mul-right m)))))
    ; – IN –
    (main? bve0)))

(check-expect (numeric? 5)
              #true)
(check-expect (numeric? 'x)
              #false)
(check-expect (numeric? (make-add 3 4))
              #true)
(check-expect (numeric? (make-add 5 'y))
              #false)
(check-expect (numeric? (make-mul 8 9))
              #true)
(check-expect (numeric? (make-mul 'z 2))
              #false)
(check-expect (numeric?
               (make-mul
                (make-add
                 1
                 (make-mul
                  99
                  (make-mul 6 'r)))
                77))
              #false)
(check-expect (numeric?
               (make-mul
                (make-add
                 1
                 (make-mul
                  99
                  (make-mul 6 3)))
                77))
              #true)

; from ex352.rkt ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; BSL-var-expr Symbol Number -> BSL-var-expr
; Replace all occurences of x in ex with v.
(define (subs ex x v)
  (cond
    [(number? ex) ex]
    [(symbol? ex)
     (if (symbol=? ex x)
         v
         ex)]
    [(add? ex)
     (make-add (subs (add-left ex) x v)
               (subs (add-right ex) x v))]
    [(mul? ex)
     (make-mul (subs (mul-left ex) x v)
               (subs (mul-right ex) x v))]))

(check-expect (subs 4 'z 6)
              4)
(check-expect (subs 'x 'x 5)
              5)
(check-expect (subs (make-add 'x 3) 'x 6)
              (make-add 6 3))
(check-expect (subs (make-mul 1/2 (make-mul 'x 3)) 'x 7)
              (make-mul 1/2 (make-mul 7 3)))
(check-expect (subs (make-add (make-mul 'x 'x)
                              (make-mul 'y 'y))
                    'y 9)
              (make-add (make-mul 'x 'x)
                        (make-mul '9 '9)))

; from ex347.rkt ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; BSL-expr -> BSL-val
; Compute the value of bslexpr.
(define (eval-expression bslexpr)
  (local (; BSL-expr -> BSL-val
          ; Compute the value of bsle.
          (define (eval-bsle bsle)
            (cond
              [(number? bsle) bsle]
              [(add? bsle) (eval-add bsle)]
              [(mul? bsle) (eval-mul bsle)]))

          ; Add -> BSL-val
          ; Compute the value of a.
          (define (eval-add a)
            (+ (eval-bsle (add-left a))
               (eval-bsle (add-right a))))

          ; Mul -> BSL-val
          ; Compute the value of m.
          (define (eval-mul m)
            (* (eval-bsle (mul-left m))
               (eval-bsle (mul-right m)))))
    ; – IN –
    (eval-bsle bslexpr)))


(check-expect (eval-expression (make-add 10 -10))
              (+ 10 -10))
(check-expect (eval-expression (make-add (make-mul 20 3) 33))
              (+ (* 20 3) 33))
(check-expect (eval-expression (make-add
                                (make-mul 3.14
                                          (make-mul 2 3))
                                (make-mul 3.14
                                          (make-mul -1 -9))))
              (+ (* 3.14 (* 2 3)) (* 3.14 (* -1 -9))))
(check-expect (eval-expression (make-add -1 2))
              (+ -1 2))
(check-expect (eval-expression (make-add (make-mul -2 -3) 33))
              (+ (* -2 -3) 33))
(check-expect (eval-expression (make-mul
                                (make-add 1
                                          (make-mul 2 3))
                                3.14))
              (* (+ 1 (* 2 3)) 3.14))