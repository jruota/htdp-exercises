;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex149) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; An N is one of: 
; – 0
; – (add1 N)
; interpretation represents the counting numbers

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; N String -> List-of-strings 
; creates a list of n copies of s
(define (copier n s)
  (cond
    [(zero? n) '()]
    [(positive? n) (cons s (copier (sub1 n) s))]))

(check-expect (copier 0 "hello") '())
(check-expect (copier 2 "hello")
              (cons "hello" (cons "hello" '())))

; N String -> List-of-strings 
; creates a list of n copies of s
(define (copier.v2 n s)
  (cond
    [(zero? n) '()]
    [else (cons s (copier.v2 (sub1 n) s))]))

(check-expect (copier.v2 0 "hello") '())
(check-expect (copier.v2 2 "hello")
              (cons "hello" (cons "hello" '())))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

; Does copier function properly when you apply it to a natural number and a
; Boolean or an image? Or do you have to design another function?

; The functions works for all types of data, because it does not use any
; primitive or functions specific to strings. Only the use of natural
; numbers is mandatory.

(copier 5 #true)
(copier 9 (circle 5 "solid" "orange"))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

; How do copier and copier.v2 behave when you apply them to 0.1 and "x"?
; Explain.

; When called with 0.1 the function copier.v2 will never stop since n never
; reaches the value 0, while copier will raise an error since all conditions
; will eventually be false.

;(copier 0.1 "x")
;(copier.v2 0.1 "x")
