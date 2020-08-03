;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex168) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A List-of-Posn is one of:
; – '()
; – (cons Posn List-of-Posn)
; Interpretation:
;     A list containing Posns.

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; List-of-Posn -> List-of-Posn
; Translate every pointin lop by +1 in
; the y-direction.
(define (translate lop)
  (cond
    [(empty? lop) '()]
    [(cons? lop)
     (cons (make-posn (posn-x (first lop))
                      (+ 1 (posn-y (first lop))))
           (translate (rest lop)))]))

(check-expect (translate '())
              '())
(check-expect (translate (cons (make-posn 1 2)
                               (cons (make-posn 2 3)
                                     (cons (make-posn 3 4)
                                           (cons (make-posn 4 5) '())))))
              (cons (make-posn 1 3)
                    (cons (make-posn 2 4)
                          (cons (make-posn 3 5)
                                (cons (make-posn 4 6) '())))))