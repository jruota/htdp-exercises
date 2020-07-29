;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex143) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A List-of-temperatures is one of: 
; – '()
; – (cons CTemperature List-of-temperatures)
 
; A CTemperature is a Number greater than -272.

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; List-of-temperatures -> Number
; Compute the average temperature if
; alot is non-empty. Raise an
; error otherwise.
(define (checked-average alot)
  (cond
    [(empty? alot)
     (error "Non-empty list expected, empty list given.")]
    [(cons? alot)
     (/ (sum alot) (how-many alot))]))

(check-error (checked-average '())
             "Non-empty list expected, empty list given.")
(check-expect (checked-average (cons 1 (cons 2 (cons 3 '()))))
              2)

; List-of-temperatures -> Number
; computes the average temperature 
(define (average alot)
  (/ (sum alot) (how-many alot)))

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