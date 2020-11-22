;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex241) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define ABSOLUTE0 -272)
; A CTemperature is a Number greater than ABSOLUTE0.

; A NEList-of-temperatures is one of: 
; – (cons CTemperature '())
; – (cons CTemperature NEList-of-temperatures)
; Interpretation:
;     Non-empty lists of Celsius temperatures.

; A NEList-of-Booleans is one of:
; – (cons Boolean '())
; – (cons Boolean NEList-of-Booleans)
; Interpretation:
;     The collection of all non-empty lists
;     containing boolean values.

; A [NEList-of-STUFF] is one of:
; – (cons STUFF '())
; – (cons STUFF [NEList-of-STUFF]
; Interpretation:
;     Non-empty collections of arbitrary size.