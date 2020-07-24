;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex111) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct vec [x y])
; A Vec is a structure:
;     (make-vec PositiveNumber PositiveNumber)
; Interpretation:
;     represents a velocity vector.

(define ERROR-MSG "make-vec: expects two positive numbers")

; Any Any -> Vec / Error
; Enforce the data definition of vec.
(define (checked-make-vec x y)
  (cond
    [(and (number? x)
          (number? y)
          (positive? x)
          (positive? y))
     (make-vec x y)]
    [else (error ERROR-MSG)]))

(check-expect (checked-make-vec 1 2)
              (make-vec 1 2))
(check-error (checked-make-vec 1 -2)
             ERROR-MSG)
(check-error (checked-make-vec 1 "2")
             ERROR-MSG)