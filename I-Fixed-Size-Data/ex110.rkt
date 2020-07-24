;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex110) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define ERROR-MSG "area-of-disk: positive number expected")

; Number -> Number
; computes the area of a disk with radius r
(define (area-of-disk r)
  (* 3.14 (* r r)))

; Any -> Number
; computes the area of a disk with radius v, 
; if v is a number
(define (checked-area-of-disk v)
  (cond
    [(and (number? v) (positive? v))
     (area-of-disk v)]
    [else (error "area-of-disk: positive number expected")]))

(check-expect (checked-area-of-disk 1)
              3.14)
(check-error (checked-area-of-disk -1)
             ERROR-MSG)
(check-error (checked-area-of-disk "hello")
             ERROR-MSG)
(check-error (checked-area-of-disk (make-posn 1 2))
             ERROR-MSG)