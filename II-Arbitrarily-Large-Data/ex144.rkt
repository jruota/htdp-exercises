;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex144) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; An NEList-of-temperatures is one of: 
; – (cons CTemperature '())
; – (cons CTemperature NEList-of-temperatures)
; interpretation non-empty lists of Celsius temperatures 

; A List-of-temperatures is one of: 
; – '()
; – (cons CTemperature List-of-temperatures)
 
; A CTemperature is a Number greater than -272.

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; NEList-of-temperatures -> Number
; computes the average temperature
(define (average ne-l)
  (/ (sum ne-l)
     (how-many ne-l)))
 
(check-expect (average (cons 1 (cons 2 (cons 3 '()))))
              2)

; List-of-temperatures -> Number 
; adds up the temperatures on the given list 
(define (sum alot)
  (cond
    [(empty? alot) 0]
    [else (+ (first alot) (sum (rest alot)))]))

(check-expect (sum '())
              0)
(check-expect (sum (cons 1 (cons 2 (cons 3'()))))
              6)
 
; List-of-temperatures -> Number 
; counts the temperatures on the given list 
(define (how-many alot)
  (cond
    [(empty? alot) 0]
    [else (+ (how-many (rest alot)) 1)]))

(check-expect (how-many '())
              0)
(check-expect (how-many (cons 1 (cons 2 (cons 3 '()))))
              3)

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Will sum and how-many work for NEList-of-temperatures even though they are
; designed for inputs from List-of-temperatures? If you think they don’t work,
; provide counter-examples. If you think they would, explain why.

; "sum" and "how-many" work for NEList-of-temperatures as well, since they
; work for List-of-temperatures of which NEList-of-temperatures is a
; subset.