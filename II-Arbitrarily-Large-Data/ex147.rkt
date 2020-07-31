;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex147) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A NEList-of-Booleans is one of:
; – (cons Boolean '())
; – (cons Boolean NEList-of-Booleans)
; Interpretation:
;     The collection of all non-empty lists
;     containing boolean values.

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; NEList-of-Booleans -> Boolean
; Determine whether all elements
; of nelob are #true.
(define (all-true nelob)
  (cond
    [(empty? (rest nelob)) (first nelob)]
    [else
     (and (first nelob)
          (all-true (rest nelob)))]))

(check-expect (all-true (cons #true '()))
              #true)
(check-expect (all-true (cons #false '()))
              #false)
(check-expect (all-true (cons #true (cons #false '())))
              #false)
(check-expect (all-true (cons #true (cons #true (cons #true '()))))
              #true)
(check-expect (all-true (cons #true (cons #true (cons #false '()))))
              #false)

; NEList-of-Booleans -> Boolean
; Determine whether at least one
; element of nelob is #true.
(define (one-true nelob)
  (cond
    [(empty? (rest nelob)) (first nelob)]
    [else
     (or (first nelob)
         (one-true (rest nelob)))]))

(check-expect (one-true (cons #true '()))
              #true)
(check-expect (one-true (cons #false '()))
              #false)
(check-expect (one-true (cons #true (cons #false '())))
              #true)
(check-expect (one-true (cons #false
                              (cons #false
                                    (cons #false
                                          (cons #false
                                                (cons #true '()))))))
              #true)
(check-expect (one-true (cons #false
                              (cons #false
                                    (cons #false
                                          (cons #false
                                                (cons #false '()))))))
              #false)