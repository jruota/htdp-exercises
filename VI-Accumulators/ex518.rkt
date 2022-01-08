;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex518) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define-struct pair [left right])
; ConsOrEmpty is one of: 
; – '()
; – (make-pair Any ConsOrEmpty)

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Any ConsOrEmpty -> ConsOrEmpty
(define (our-cons a-value a-list)
  (cond
    [(empty? a-list) (make-pair a-value a-list)]
    [(pair? a-list) (make-pair a-value a-list)]
    [else (error "our-cons: ...")]))
 
; ConsOrEmpty -> Any
; extracts the left part of the given pair
(define (our-first mimicked-list)
  (if (empty? mimicked-list)
      (error "our-first: ...")
      (pair-left mimicked-list)))

; ConsOrEmpty -> Any
; Extract the rest of a non-empty pair.
(define (our-rest mimicked-list)
  (if (empty? mimicked-list)
      (error "our-rest: ...")
      (pair-right mimicked-list)))

(check-error (our-rest '())
             "our-rest: ...")
(check-expect (our-rest (make-pair 1 (make-pair 2 (make-pair 3 '()))))
              (make-pair 2 (make-pair 3 '())))

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Argue that our-cons takes a constant amount of time to compute its result,
; regardless of the size of its input.

    ; Given the definition of "on the order of ..." (see section 'The Definition
    ; of "On the Oder Of"), our-cons(n) <= 1 for any n.

    ; At worst, a-list is not a ConsOrEmpty and the function has to go through
    ; three conditional clauses and then call the error function.
    ; In any case, it does never recur.
