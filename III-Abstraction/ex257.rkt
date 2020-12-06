;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex257) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A NatNumber is one of:
;     – 0
;     – (add1 NatNumber)
; Interpretation:
;     The natural numbers used for counting.

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; [X] NatNumber [NatNumber -> X] -> [List-of X]
; Constructs a list by applying f
; to the numbers between 0 and (- n 1).
(define (build-l*st n f)
  (cond
    [(negative? (sub1 n)) '()]
    [else
     (add-at-end (f (sub1 n))
                 (build-l*st (sub1 n) f))]))

(check-expect (build-l*st 0 add1)
              '())
(check-expect (build-l*st 22 add1)
              (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22))

; [X] X [List-of X] -> [List-of X]
; Add x at the end of lox.
(define (add-at-end x lox)
  (cond
    [(empty? lox)
     (list x)]
    [(cons? lox)
     (cons (first lox)
           (add-at-end x (rest lox)))]))

(check-expect (add-at-end 1 '())
              (list 1))
(check-expect (add-at-end 1 (list 0 0 0 0 0 0))
              (list 0 0 0 0 0 0 1))