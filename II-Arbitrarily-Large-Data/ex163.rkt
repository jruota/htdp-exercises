;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex163) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; SOURCE ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; https://en.wikipedia.org/wiki/Fahrenheit

; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A Fahrenheit is a Number greater than
; or equal to -459.67.

; A Celsius is a Number greater than
; or equal to -273.15.

; A list of FMeasurements is one of:
; – '()
; – (cons Fahrenheit FMeasurements)
; Interpretation:
;     A collection of temperature values
;     in degrees Fahrenheit.

; A list of CMeasurements is one of:
; – '()
; – (cons Celsius CMeasurements)
; Interpretation:
;     A collection of temperature values
;     in degrees Celsius.

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; FMeasurements -> CMeasurements
; Convert the Fahrenheit values in fm
; to Celsius values.
(define (convertFC fm)
  (cond
    [(empty? fm) '()]
    [(cons? fm)
     (cons (f->c (first fm))
           (convertFC (rest fm)))]))

(check-expect (convertFC '())
              '())
(check-expect (convertFC (cons -40 (cons 50 '())))
              (cons -40 (cons 10 '())))

; Fahrenheit -> Celsius
; Convert f from degrees Fahrenheit
; to degrees Celsius.
(define (f->c f)
  (* (- f 32) 5/9))

(check-expect (f->c -40)
              -40)
(check-expect (f->c 50)
              10)