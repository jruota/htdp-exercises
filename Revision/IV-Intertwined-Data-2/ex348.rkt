;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex348) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define-struct AND [left right])
; An AND is a structure:
;     (make-AND Boolean-BSL-expr Boolean-BSL-expr)
; Interpretation:
;     Represents the boolean "and" operator.

(define-struct OR [left right])
; An OR is a structure:
;     (make-OR Boolean-BSL-expr Boolean-BSL-expr)
; Interpretation:
;     Represents the boolean "or" operator.

(define-struct NOT [val])
; An NOT is a structure:
;     (make-NOT Boolean-BSL-expr)
; Interpretation:
;     Represents the boolean "not" operator.

; A Boolean-BSL-expr is one of:
; – #true
; – #false
; – AND
; – OR
; – NOT
; Interpretation
;     Represents boolean values or expressions.

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Boolean-BSL-expr -> Boolean
; Compute the value of expr.
(define (eval-bool-expression expr)
  (cond
    [(boolean? expr) expr]
    [(AND? expr)
     (and (eval-bool-expression (AND-left expr))
          (eval-bool-expression (AND-right expr)))]
    [(OR? expr)
     (or (eval-bool-expression (OR-left expr))
         (eval-bool-expression (OR-right expr)))]
    [(NOT? expr)
     (not (eval-bool-expression (NOT-val expr)))]))

(check-expect (eval-bool-expression #true)
              #true)
(check-expect (eval-bool-expression #false)
              #false)
(check-expect (eval-bool-expression (make-AND #true #true))
              #true)
(check-expect (eval-bool-expression (make-AND #true #false))
              #false)
(check-expect (eval-bool-expression (make-OR #false #true))
              #true)
(check-expect (eval-bool-expression (make-OR #false #false))
              #false)
(check-expect (eval-bool-expression (make-NOT #false))
              #true)
(check-expect (eval-bool-expression (make-NOT #true))
              #false)
(check-expect (eval-bool-expression
               (make-AND #true
                    (make-OR #false
                        (make-NOT (make-OR #false #false)))))
              #true)
