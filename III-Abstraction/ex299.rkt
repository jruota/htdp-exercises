;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex299) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A FiniteSet is one of:
;     – '()
;     – (cons X FiniteSet)
; Interpretation:
;     A collection of X's, where the X's share
;     one or more properties.
; Examples:
;     – (list 0 1 2 3 4 5)
;       finite set of all natural numbers up to 5 (inclusive) and 0
;     – (list "hello" 13 (make-posn 12 13))
;       a finite set of arbitrary elements

; An InfiniteSet is a function:
;     [X -> Boolean?]
; Interpretation:
;     Checks whether a given element X satisfies a certain
;     property or properties.

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Any -> Boolean
; Represents all odd numbers.
(define (odd-numbers x)
  (cond
    [(not (integer? x))
     #false]
    [else
     (odd? x)]))

(check-expect (odd-numbers 0)
              #false)
(check-expect (odd-numbers 3.14)
              #false)
(check-expect (odd-numbers 4)
              #false)
(check-expect (odd-numbers "hello world")
              #false)

(check-expect (odd-numbers 3)
              #true)

; Any -> Boolean
; Represents all even numbers.
(define (even-numbers x)
  (cond
    [(not (integer? x))
     #false]
    [else
     (even? x)]))

(check-expect (even-numbers 3.14)
              #false)
(check-expect (even-numbers 5)
              #false)
(check-expect (even-numbers "hello world")
              #false)

(check-expect (even-numbers 0)
              #true)
(check-expect (even-numbers 6)
              #true)

; Any -> Boolean
; Represents all numbers divisible by 10.
(define (divisible-by-10 x)
  (cond
    [(not (integer? x))
     #false]
    [else
     (= (modulo x 10) 0)]))

(check-expect (divisible-by-10 99)
              #false)
(check-expect (divisible-by-10 420.23)
              #false)
(check-expect (divisible-by-10 (make-posn 20 30))
              #false)                               

(check-expect (divisible-by-10 240)
              #true)
(check-expect (divisible-by-10 0)
              #true)
(check-expect (divisible-by-10 -110)
              #true)

; Any InfiniteSet -> InfiniteSet
; Add element x to the infinite set is.
(define (add-element x is)
  (lambda (y)
    (or (equal? x y)
        (is y))))

(check-expect ((add-element 7 even-numbers) 7)
              #true)
(check-expect ((add-element "hello" odd-numbers) "world")
              #false)

; InfiniteSet InfiniteSet -> InfiniteSet
; Create a union of sets is1 and is2.
(define (union s1 s2)
  (lambda (x)
    (or (s1 x) (s2 x))))

; natural numbers
(check-expect ((union odd-numbers even-numbers) 3)
              #true)
; odd-numbers and numbers divisible by ten
(check-expect ((union odd-numbers divisible-by-10) 30)
              #true)
; odd-numbers and numbers divisible by ten
(check-expect ((union odd-numbers divisible-by-10) 32)
              #false)

; InfiniteSet InfiniteSet -> InfiniteSet
; Create an intersection of sets s1 and s2.
(define (intersect s1 s2)
  (lambda (x)
    (and (s1 x) (s2 x))))

; there are no such numbers
(check-expect ((intersect even-numbers odd-numbers) 3)
              #false)
; all even numbers that are divisible by ten
(check-expect ((intersect even-numbers divisible-by-10) 120)
              #true)