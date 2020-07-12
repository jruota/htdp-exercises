;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex73) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Posn Number -> Posn
; Produce a Posn like p with n in the x field.
(define (posn-up-x p n)
  (make-posn n (posn-y p)))

(check-expect (posn-up-x (make-posn 0 0) 1)
              (make-posn 1 0))