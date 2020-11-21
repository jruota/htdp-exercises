;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex236) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; An Lon (List-of-numbers) 
; is one of:
;     – '() 
;     – (cons Number Lon)
; Interpretation:
;     A list of numbers.

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Lon -> Lon
; Add 1 to each item on l.
(define (add1* l)
  (cond
    [(empty? l) '()]
    [else
     (cons
       (add1 (first l))
       (add1* (rest l)))]))

(check-expect (add1* '())
              '())
(check-expect (add1* '(0 1 2 3 4 5 6 7 8 9))
              '(1 2 3 4 5 6 7 8 9 10))

; Lon -> Lon
; Add 1 to each item on l.
(define (add1.v2 l)
  (add-n 1 l))

(check-expect (add1.v2 '())
              '())
(check-expect (add1.v2 '(0 1 2 3 4 5 6 7 8 9))
              '(1 2 3 4 5 6 7 8 9 10))

; Lon -> Lon
; Add 5 to each item on l.
(define (plus5 l)
  (cond
    [(empty? l) '()]
    [else
     (cons
       (+ (first l) 5)
       (plus5 (rest l)))]))

(check-expect (plus5 '())
              '())
(check-expect (plus5 '(0 1 2 3 4 5 6 7 8 9))
              '(5 6 7 8 9 10 11 12 13 14))

; Lon -> Lon
; Add 5 to each item on l.
(define (plus5.v2 l)
  (add-n 5 l))

(check-expect (plus5.v2 '())
              '())
(check-expect (plus5.v2 '(0 1 2 3 4 5 6 7 8 9))
              '(5 6 7 8 9 10 11 12 13 14))

; Lon -> Lon
; Subtract 2 from each item on lon.
(define (subtract-2 lon)
  (add-n -2 lon))

(check-expect (subtract-2 '())
              '())
(check-expect (subtract-2 '(0 1 2 3 4 5 6 7 8 9))
              '(-2 -1 0 1 2 3 4 5 6 7))

; Number Lon -> Lon
; Add n to each element of lon.
(define (add-n n lon)
  (cond
    [(empty? lon) '()]
    [(cons? lon)
     (cons (+ n (first lon))
           (add-n n (rest lon)))]))

(check-expect (add-n 3 '())
              '())
(check-expect (add-n 3 '(3 5 7))
              '(6 8 10))
