;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex148) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A List-of-temperatures is one of: 
; – '()
; – (cons CTemperature List-of-temperatures)
 
; A CTemperature is a Number greater than -272.

; An NEList-of-temperatures is one of: 
; – (cons CTemperature '())
; – (cons CTemperature NEList-of-temperatures)
; interpretation non-empty lists of Celsius temperatures

; A List-of-Booleans is one of:
; – '()
; – (cons Boolean List-of-Booleans)
; Interpretation:
;     The collection of all lists containing
;     Booleans only.

; A NEList-of-Booleans is one of:
; – (cons Boolean '())
; – (cons Boolean NEList-of-Booleans)
; Interpretation:
;     The collection of all non-empty lists
;     containing boolean values.

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; List-of-temperatures -> Number 
; adds up the temperatures on the given list 
(define (sum.v1 alot)
  (cond
    [(empty? alot) 0]
    [else (+ (first alot) (sum.v1 (rest alot)))]))

; NEList-of-temperatures -> Number
; computes the sum of the given temperatures 
(define (sum.v2 ne-l)
  (cond
    [(empty? (rest ne-l)) (first ne-l)]
    [else (+ (first ne-l) (sum.v2 (rest ne-l)))]))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

; List-of-temperatures -> Number 
; counts the temperatures on the given list 
(define (how-many.v1 alot)
  (cond
    [(empty? alot) 0]
    [else (+ (how-many.v1 (rest alot)) 1)]))

; NEList-of-temperatures -> Number
; Count how many elements there
; are in nelot.
(define (how-many.v2 nelot)
  (cond
    [(empty? (rest nelot)) 1]
    [else
     (+ 1 (how-many.v2 (rest nelot)))]))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

; List-of-Booleans -> Boolean
; Determine whether all elements
; of lob are #true.
(define (all-true.v1 lob)
  (cond
    [(empty? lob) #true]
    [(cons? lob)
     (and (first lob)
          (all-true.v1 (rest lob)))]))

; NEList-of-Booleans -> Boolean
; Determine whether all elements
; of nelob are #true.
(define (all-true.v2 nelob)
  (cond
    [(empty? (rest nelob)) (first nelob)]
    [else
     (and (first nelob)
          (all-true.v2 (rest nelob)))]))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

; List-of-Booleans -> Boolean
; Determine whether at least one
; element of lob is #true.
(define (one-true.v1 lob)
  (cond
    [(empty? lob) #false]
    [(cons? lob)
     (or (first lob)
         (one-true.v1 (rest lob)))]))

; NEList-of-Booleans -> Boolean
; Determine whether at least one
; element of nelob is #true.
(define (one-true.v2 nelob)
  (cond
    [(empty? (rest nelob)) (first nelob)]
    [else
     (or (first nelob)
         (one-true.v2 (rest nelob)))]))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

; Is it better to work with data definitions that accommodate empty lists as
; opposed to definitions for non-empty lists? Why? Why not?

; The functions that work with empty lists would also work for the non-empty
; counterparts. Thus they are more general.
;
; The functions that work on non-empty data have a right of being in
; circumstances where the use of non-empty data must be enforced or
; where "empty" data does not make any sense.
;
; The only difference between the two types of functions is their first
; conditional clause, mirroring the difference in the data definition.