;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex169) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A List-of-Posn is one of:
; – '()
; – (cons Posn List-of-Posn)
; Interpretation:
;     A list containing Posns.

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; List-of-Posn -> List-of-Posn
; Remove all elements from lop that do
; not fulfill the following constraints:
; – 0 <= x <= 100,
; – 0 <= y <= 200.
(define (legal lop)
  (cond
    [(empty? lop) '()]
    [(cons? lop)
     (if (and (x-limits? (first lop))
              (y-limits? (first lop)))
         (cons (first lop)
               (legal (rest lop)))
         (legal (rest lop)))]))

(check-expect (legal '())
              '())
(check-expect (legal (cons (make-posn 0 200)
                           (cons (make-posn -1 155)
                                 (cons (make-posn 99 201)
                                       (cons (make-posn 99 199)
                                             (cons (make-posn 0 0) '()))))))
              (cons (make-posn 0 200)
                    (cons (make-posn 99 199)
                          (cons (make-posn 0 0) '()))))

; Posn -> Boolean
; Check whether the x-coordinate
; fulfills the constraint
; 0 <= x <= 100.
(define (x-limits? p)
  (and (>= (posn-x p) 0)
       (<= (posn-x p) 100)))

(check-expect (x-limits? (make-posn -1 0))
              #false)
(check-expect (x-limits? (make-posn 101 0))
              #false)
(check-expect (x-limits? (make-posn 51 0))
              #true)

; Posn -> Boolean
; Check whether the y-coordinate
; fulfills the constraint
; 0 <= x <= 200.
(define (y-limits? p)
  (and (>= (posn-y p) 0)
       (<= (posn-y p) 200)))

(check-expect (y-limits? (make-posn 0 -1))
              #false)
(check-expect (y-limits? (make-posn 0 201))
              #false)
(check-expect (y-limits? (make-posn 0 101))
              #true)