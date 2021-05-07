;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex355) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

; An AL (short for association list) is [List-of Association].

; An Association is a list of two items:
;   (cons Symbol (cons Number '())).

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define WRONG "element not in lookup table")

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; BSL-var-expr AL -> Number
; Traverse ex and replace all symbols
; with their respective values as specified
; by da. If there is not corresponding entry
; in da, signal an error.
(define (eval-var-lookup ex da)
  (local (; BSL-var-expr -> Number
          ; See purpose statement for eval-var-lookup.
          (define (main ex1)
            (cond
              [(number? ex1) ex1]
              [(symbol? ex1) (symbol-lookup ex1)]
              [(add? ex1) (add-lookup ex1)]
              [(mul? ex1) (mul-lookup ex1)]))

          ; Symbol -> Number
          ; Replace s with its corresponding
          ; value from da. If there is no such
          ; entry in da, signal an error.
          (define (symbol-lookup s)
            (local ((define AorF (assq s da)))
              ; – IN –
              (if (boolean? AorF)
                  (error WRONG)
                  (second AorF))))

          ; Add -> Number
          ; Return the value of a.
          (define (add-lookup a)
            (+ (main (add-left a))
               (main (add-right a))))

          ; Mul -> Number
          ; Return the value of m.
          (define (mul-lookup m)
            (* (main (mul-left m))
               (main (mul-right m)))))
    ; – IN –
    (main ex)))
;    (cond
;      [(number? ex) ex]
;      [(symbol? ex)
;       (if (boolean? (assq ex da))
;           (error WRONG)
;           (second (assq ex da)))]
;      [(add? ex)
;       (+ (eval-var-lookup (add-left ex) da)
;          (eval-var-lookup (add-right ex) da))]
;      [(mul? ex)
;       (* (eval-var-lookup (mul-left ex) da)
;          (eval-var-lookup (mul-right ex) da))])))

(check-expect (eval-var-lookup
               (make-mul 3 (make-add 4 (make-mul 5 (make-add 6 7))))
               '())
              207)
(check-expect (eval-var-lookup
               (make-mul 3 (make-add 4 (make-mul 5 (make-add 6 7))))
               (list (list 'x 3) (list 'y 6) (list 'z 5) (list 'u 4)))
              207)
(check-expect (eval-var-lookup
               (make-mul 'x (make-add 'u (make-mul 'z (make-add 'y 7))))
               (list (list 'x 3) (list 'y 6) (list 'z 5) (list 'u 4)))
              207)
(check-error (eval-var-lookup
              (make-mul 'x (make-add 'w (make-mul 'z (make-add 'y 7))))
              (list (list 'x 3) (list 'y 6) (list 'z 5) (list 'u 4)))
             WRONG)
(check-error (eval-var-lookup
              (make-mul 'x (make-add 'u (make-mul 'z (make-add 'y 7))))
              (list (list 'x 3) (list 'y 6) (list 'z 5) (list 'w 4)))
             WRONG)