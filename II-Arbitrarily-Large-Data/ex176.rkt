;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex176) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A Matrix is one of: 
;  – (cons Row '())
;  – (cons Row Matrix)
; constraint all rows in matrix are of the same length
 
; A Row is one of: 
;  – '() 
;  – (cons Number Row)

; A List-of-Numbers is one of:
; – '()
; – (cons Number '())
; Interpretation:
;     The collection of lists containing numbers.

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define row1 (cons 11 (cons 12 '())))
(define row2 (cons 21 (cons 22 '())))
(define mat1 (cons row1 (cons row2 '())))

(define wor1 (cons 11 (cons 21 '())))
(define wor2 (cons 12 (cons 22 '())))
(define tam1 (cons wor1 (cons wor2 '())))

(define reihe1 (cons 11 (cons 12 (cons 13 '()))))
(define reihe2 (cons 21 (cons 22 (cons 23 '()))))
(define reihe3 (cons 31 (cons 32 (cons 33 '()))))
(define matrize (cons reihe1 (cons reihe2 (cons reihe3 '()))))

(define eheir1 (cons 11 (cons 21 (cons 31 '()))))
(define eheir2 (cons 12 (cons 22 (cons 32 '()))))
(define eheir3 (cons 13 (cons 23 (cons 33 '()))))
(define ezirtam (cons eheir1 (cons eheir2 (cons eheir3 '()))))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; – – – – – – – – – – – – – – – – – – – – – – – – – – – – – – – – – – – – – – –

; Why does transpose ask (empty? (first lln))?

; Based off of the data definition Matrix, one could define an "empty"
; Matrix.

; An EmptyMatrix is one of:
; – (cons '() '())
; – (cons '() EmptyMatrix)

; This means that empty matrices are represented as lists containing
; empty lists, at least with length 1.

; With the constraint that all rows are of the same length, checking for
; an empty matrix is equivalent to checking whether the first row of a
; given matrix is empty.

; ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

; You should also understand that you cannot design this function with the
; design recipes you have seen so far. Explain why.

; Functions dealing with elements of Matrix need to mirror the data definition.
; Since lists are used to represent matrices, and since the data definition
; is self-referential in the second clause, transpose needs to call itself
; on (rest m) in the second cond-clause where m is an element of Matrix.
; But since (rest m) would not give a useful result, one needs to use
; (rest* m) instead, which is not the structural recursion implied by the data
; definition. Thus one cannot use the design recipe as introduced so far.

; – – – – – – – – – – – – – – – – – – – – – – – – – – – – – – – – – – – – – – –

; Matrix -> Matrix
; transposes the given matrix along the diagonal 

(define (transpose lln)
  (cond
    [(empty? (first lln)) '()]
    [else (cons (first* lln) (transpose (rest* lln)))]))

(check-expect (transpose (cons '() '()))
              '())    ; '() is not a matrix, signature violation
(check-expect (transpose mat1) tam1)
(check-expect (transpose matrize)
              ezirtam)

; Matrix -> List-of-Numbers
; Produce the first column of m as a list of numbers.
(define (first* m)
  (cond
    [(or (empty? m) (empty? (first m))) '()]
    [else
     (cons (first (first m))
           (first* (rest m)))]))

(check-expect (first* (cons '() '()))
              '())
(check-expect (first* mat1)
              (cons 11 (cons 21 '())))

; Matrix -> Matrix
; Remove the first column from m.
(define (rest* m)
  (cond
    [(or (empty? m) (empty? (first m))) m]
    [else
     (cons (rest (first m))
           (rest* (rest m)))]))

(check-expect (rest* (cons '() '()))
              (cons '() '()))
(check-expect (rest* mat1)
              (cons (rest row1) (cons (rest row2) '())))