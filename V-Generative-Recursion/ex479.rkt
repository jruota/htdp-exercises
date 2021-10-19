;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex479) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define QUEENS 8)
; A QP is a structure:
;   (make-posn CI CI)
; A CI is an N in [0,QUEENS).
; interpretation (make-posn r c) denotes the square at 
; the r-th row and c-th column

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; QP QP -> Boolean
; Do the queens placed at one and two
; threaten each other?
(define (threatening? one two)
  (or
   ; horizontal
   (= (posn-x one) (posn-x two))
   ; vertical
   (= (posn-y one) (posn-y two))
   ; diagonal left top to right bottom
   (= (abs (- (posn-x one) (posn-y one)))
      (abs (- (posn-x two) (posn-y two))))
   ; diagonal left bottom to right top
   (= (+ (posn-x one) (posn-y one))
      (+ (posn-x two) (posn-y two)))))

(check-expect (threatening? (make-posn 5 1) (make-posn 5 7))
              #true)
(check-expect (threatening? (make-posn 0 6) (make-posn 5 1))
              #true)
(check-expect (threatening? (make-posn 5 1) (make-posn 0 1))
              #true)
(check-expect (threatening? (make-posn 4 0) (make-posn 5 1))
              #true)
(check-expect (threatening? (make-posn 5 1) (make-posn 5 0))
              #true)
(check-expect (threatening? (make-posn 6 0) (make-posn 5 1))
              #true)
(check-expect (threatening? (make-posn 5 1) (make-posn 7 1))
              #true)
(check-expect (threatening? (make-posn 7 3) (make-posn 5 1))
              #true)

(check-expect (threatening? (make-posn 5 1) (make-posn 3 6))
              #false)
(check-expect (threatening? (make-posn 1 4) (make-posn 5 1))
              #false)
(check-expect (threatening? (make-posn 5 1) (make-posn 0 0))
              #false)
(check-expect (threatening? (make-posn 0 7) (make-posn 5 1))
              #false)
(check-expect (threatening? (make-posn 5 1) (make-posn 7 2))
              #false)
(check-expect (threatening? (make-posn 6 6) (make-posn 5 1))
              #false)
