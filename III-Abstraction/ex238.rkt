;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex238) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A Nelon is one of:
;     – (cons Number '())
;     – (cons Number Nelon)
; Interpretation:
;     A non-empty list of numbers.

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; for testing

(define ONE
  (list 25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1))

(define TWO
  (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Nelon -> Number
; determines the smallest 
; number on l
(define (inf l)
  (cond
    [(empty? (rest l))
     (first l)]
    [else
     (if (< (first l)
            (inf (rest l)))
         (first l)
         (inf (rest l)))]))

(check-expect (inf '(656))
              656)
(check-expect (inf '(203 375 656 364 59 467))
              59)

; Nelon -> Number
; Determine the smallest number on l.
(define (inf.v2 l)
  (cond
    [(empty? (rest l))
     (first l)]
    [else
     (min (first l)
          (inf (rest l)))]))

(check-expect (inf.v2 '(656))
              656)
(check-expect (inf.v2 '(203 375 656 364 59 467))
              59)

; Nelon -> Number
; Determine the smallest number on l.
(define (inf-1 l)
  (extr < l))

(check-expect (inf-1 '(203 375 656 364 59 467))
              59)

; Nelon -> Number
; Determine the smallest number on l.
(define (inf-2 l)
  (extr.v2 min l))

(check-expect (inf-2 '(203 375 656 364 59 467))
              59)

; Nelon -> Number
; determines the largest 
; number on l
(define (sup l)
  (cond
    [(empty? (rest l))
     (first l)]
    [else
     (if (> (first l)
            (sup (rest l)))
         (first l)
         (sup (rest l)))]))

(check-expect (sup '(59))
              59)
(check-expect (sup '(203 375 656 364 59 467))
              656)

; Nelon -> Number
; Determine the largest number on l.
(define (sup.v2 l)
  (cond
    [(empty? (rest l))
     (first l)]
    [else
     (max (first l)
          (sup.v2 (rest l)))]))

(check-expect (sup.v2 '(59))
              59)
(check-expect (sup.v2 '(203 375 656 364 59 467))
              656)

; Nelon -> Number
; Determine the largest number on l.
(define (sup-1 l)
  (extr > l))

(check-expect (sup-1 '(203 375 656 364 59 467))
              656)

; Nelon -> Number
; Determine the largest number on l.
(define (sup-2 l)
  (extr.v2 max l))

(check-expect (sup-2 '(203 375 656 364 59 467))
              656)

; Nelon -> Number
; Depending on the function f passed,
; return the number that satisfies
; this function compared with all other
; numbers in l.
; The function f must return a boolean.
(define (extr f l)
  (cond
    [(empty? (rest l))
     (first l)]
    [(cons? l)
     (if (f (first l)
            (extr f (rest l)))
         (first l)
         (extr f (rest l)))]))

(check-expect (extr < '(203 375 656 364 59 467))
              59)
(check-expect (extr > '(203 375 656 364 59 467))
              656)

; Nelon -> Number
; Depending on the function f passed,
; return the number that satisfies
; this function compared with all other
; numbers in l.
; The function f must return a number.
(define (extr.v2 f l)
  (cond
    [(empty? (rest l))
     (first l)]
    [(cons? l)
     (f (first l)
        (extr.v2 f (rest l)))]))

(check-expect (extr.v2 min '(203 375 656 364 59 467))
              59)
(check-expect (extr.v2 max '(203 375 656 364 59 467))
              656)

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Why are these functions slow on some of the long lists?

; As these functions are recursive, there are as many function calls as there
; are elements in the list. Additionally, if the "right" number is at the end
; of the list, i.e. all numbers before it are greater for the "inf" function
; and all numbers before it are greater for the "sup" function, the false-clause
; of the if statement makes another function call, so that there are a total
; of (2 * (list-length) - 1) function calls.

;(inf-1 ONE)    ; slow
;(inf-1 TWO)    ; fast

;(sup-1 ONE)    ; fast
;(sup-1 TWO)    ; slow

; Why are these versions so much faster?

; Compared to the previous functions, these functions return a number and do
; not use an intermediary function that return a boolean. Thus there is no need
; for an if statement and another function call to return a number.

(inf-2 ONE)
(inf-2 TWO)

(sup-2 ONE)
(sup-2 TWO)
