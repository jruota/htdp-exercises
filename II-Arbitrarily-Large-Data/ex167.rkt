;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex167) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A List-of-Posn is one of:
; – '()
; – (cons Posn List-of-Posn)
; Interpretation:
;     A list containing Posns.

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; List-of-Posn -> Number
; Return the sum of all x-coordinates
; in lop.
(define (sum lop)
  (cond
    [(empty? lop) 0]
    [(cons? lop)
     (+ (posn-x (first lop))
        (sum (rest lop)))]))

(check-expect (sum '())
              0)
(check-expect (sum (cons (make-posn 1 2)
                         (cons (make-posn 2 3)
                               (cons (make-posn 3 4)
                                     (cons (make-posn 4 5) '())))))
              10)
