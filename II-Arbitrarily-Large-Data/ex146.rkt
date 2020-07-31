;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex146) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; An NEList-of-temperatures is one of: 
; – (cons CTemperature '())
; – (cons CTemperature NEList-of-temperatures)
; interpretation non-empty lists of Celsius temperatures

; A CTemperature is a Number greater than -272.

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; NEList-of-temperatures -> Number
; computes the average temperature 
(define (average ne-l)
  (/ (sum ne-l)
     (how-many ne-l)))

(check-expect (average (cons 1 (cons 2 (cons 3 '()))))
              2)

; NEList-of-temperatures -> Number
; Count how many elements there
; are in nelot.
(define (how-many nelot)
  (cond
    [(empty? (rest nelot)) 1]
    [else
     (+ 1 (how-many (rest nelot)))]))

(check-expect (how-many (cons 1 '()))
              1)
(check-expect (how-many (cons 1 (cons 1 (cons 1 (cons 1 (cons 1 '()))))))
              5)

; NEList-of-temperatures -> Number
; computes the sum of the given temperatures 
(define (sum ne-l)
  (cond
    [(empty? (rest ne-l)) (first ne-l)]
    [else (+ (first ne-l) (sum (rest ne-l)))]))

(check-expect (sum (cons 1 '()))
              1)
(check-expect (sum (cons 1 (cons 2 (cons 3 '()))))
              6)
