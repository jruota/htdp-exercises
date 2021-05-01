;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex352) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A BSL-var-expr is one of: 
; – Number
; – Symbol 
; – (make-add BSL-var-expr BSL-var-expr)
; – (make-mul BSL-var-expr BSL-var-expr)

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

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; BSL-var-expr Symbol Number -> BSL-var-expr
; Replace all occurrences of x in ex with v.
(define (subst ex x v)
  (local (; BSL-var-expr -> BSL-var-expr
          ; Replace all occurrences of x in ex0 with v.
          (define (main ex0)
            (cond
              [(number? ex0) (subst-number ex0)]
              [(symbol? ex0) (subst-symbol ex0)]
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
;  (cond
;    [(number? ex) ex]
;    [(symbol? ex)
;     (if (symbol=? ex x)
;         v
;         ex)]
;    [(add? ex)
;     (make-add (subst (add-left ex) x v)
;               (subst (add-right ex) x v))]
;    [(mul? ex)
;     (make-mul (subst (mul-left ex) x v)
;               (subst (mul-right ex) x v))]))

(check-expect (subst 4 'z 6)
              4)
(check-expect (subst 'x 'x 5)
              5)
(check-expect (subst (make-add 'x 3) 'x 6)
              (make-add 6 3))
(check-expect (subst (make-mul 1/2 (make-mul 'x 3)) 'x 7)
              (make-mul 1/2 (make-mul 7 3)))
(check-expect (subst (make-add (make-mul 'x 'x)
                              (make-mul 'y 'y))
                    'y 9)
              (make-add (make-mul 'x 'x)
                        (make-mul '9 '9)))
              