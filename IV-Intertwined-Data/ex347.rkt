;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex347) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

; A BSL-val is a Number.

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
