;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex428) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; An N is one of:
; – 0
; – (add1 N)
; Interpretation:
;     The natural numbers.

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define THRESHOLD 3)

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; [List-of Number] -> [List-of Number]
; produces a sorted version of alon
(define (quick-sort< alon)
  (cond
    [(empty? alon) '()]
    [(empty? (rest alon)) alon]
    [(> THRESHOLD (length alon)) (sort< alon)]
    [else
     (append (quick-sort< (smallers (rest alon) (first alon)))
             (build-list (count (first alon) alon)
                         (lambda (x) (first alon)))
             (quick-sort< (largers (rest alon) (first alon))))]))

(check-expect (quick-sort< '()) '())
(check-expect (quick-sort< (list 23)) (list 23))
(check-expect (quick-sort< (list 11 8 14 7)) (list 7 8 11 14))
(check-expect (quick-sort< (list 11 9 2 18 12 14 4 1))
              (list 1 2 4 9 11 12 14 18))

(check-expect (quick-sort< (list 11 14 9 4 2 9 18 1 12 18 11 2 12 14 4 1))
              (list 1 1 2 2 4 4 9 9 11 11 12 12 14 14 18 18))

; [List-of Number] Number -> [List-of Number]
; Return all numbers from lon that are less
; than n.
(define (smallers lon n)
  (cond
    [(empty? lon) '()]
    [else
     (if (< (first lon) n)
         (cons (first lon) (smallers (rest lon) n))
         (smallers (rest lon) n))]))

(check-expect (smallers (list 11 8 14 7) 11)
              (list 8 7))
(check-expect (smallers (list 11 9 2 18 12 14 4 1) 11)
              (list 9 2 4 1))

; [List-of Number] Number -> [List-of Number]
; Return all numbers from lon that are greater
; than n.
(define (largers lon n)
  (cond
    [(empty? lon) '()]
    [else
     (if (> (first lon) n)
         (cons (first lon) (largers (rest lon) n))
         (largers (rest lon) n))]))

(check-expect (largers (list 11 8 14 7) 11)
              (list 14))
(check-expect (largers (list 11 9 2 18 12 14 4 1) 11)
              (list 18 12 14))

; [List-of X] X -> N
; Count how often x appears in lox.
(define (count x lox)
  (cond
    [(empty? lox) 0]
    [else
     (if (equal? x (first lox))
         (+ 1 (count x (rest lox)))
         (count x (rest lox)))]))

(check-expect (count 1 '()) 0)
(check-expect (count 2 (list 1 2 3 2 4 2 5 2 6 2)) 5)
(check-expect (count "hello" (list "world" "hello" "planet" "hello" "earth")) 2)

; from figure 72 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; [List-of Number] -> [List-of Number]
; produces a sorted version of l
(define (sort< l)
  (cond
    [(empty? l) '()]
    [(cons? l) (insert (first l) (sort< (rest l)))]))
 
; Number [List-of Number] -> [List-of Number]
; inserts n into the sorted list of numbers l 
(define (insert n l)
  (cond
    [(empty? l) (cons n '())]
    [else (if (<= n (first l))
              (cons n l)
              (cons (first l) (insert n (rest l))))]))

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Since smallers and largers use strict comparison functions, a pivot number
; that occurs multiple times falls through that comparison and is reduced to
; one occurrence.
; There are two possible solutions to this problem:
;
; – chose one of the functions (i.e. smallers or largers) and change the
;   comparison function to include numbers that are equal
;   (<= or >= respectively); it is important to do this only for one function,
;   otherwise the sorting would increase the number of occurences of the pivot.
;
; – change quick-sort< itself to account for multiple occurences of the pivot.
;
; Here the second solution is preferred, as it allows to keep the strict
; comparison of smallers and largers.

(smallers (list 11 14 9 4 2 9 18 1 12 18 11 2 12 12 4 1) 11)
(length (smallers (list 11 14 9 4 2 9 18 1 12 18 11 2 12 12 4 1) 11))

(largers (list 11 14 9 4 2 9 18 1 12 18 11 2 12 12 4 1) 11)
(length (largers (list 11 14 9 4 2 9 18 1 12 18 11 2 12 12 4 1) 11))