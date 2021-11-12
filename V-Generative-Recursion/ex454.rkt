;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex454) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A Matrix is one of: 
;  – (cons Row '())
;  – (cons Row Matrix)
; Constraint: all rows in a matrix are of the same length.
; Constraint: matrices have (length Row) rows.

; A Row is a [List-of Number].

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; N [List-of Number] -> Matrix
; Return a Matrix with n rows,
; all of length n.
; Constraint: lon has to be (* n n) long.
(define (create-matrix n lon)
  (cond
    [(empty? lon) '()]
    [else
     (cons (first-row n lon)
           (create-matrix n (remove-first-row n lon)))]))

(check-expect (create-matrix 0 '())
              '())
(check-expect (create-matrix 1 (list 1))
              (list (list 1)))
(check-expect (create-matrix 2 (list 1 2 3 4))
              (list (list 1 2)
                    (list 3 4)))
(check-expect (create-matrix 3 (list 1 2 3 4 5 6 7 8 9))
              (list (list 1 2 3)
                    (list 4 5 6)
                    (list 7 8 9)))
(check-expect (create-matrix 4 (list 11 13 4 4 10 11 2 3 7 4 4 7 2 16 7 7))
              (list (list 11 13 4 4)
                    (list 10 11 2 3)
                    (list  7  4 4 7)
                    (list  2 16 7 7)))

; N [List-of Number] -> [List-of Number]
; Return the first n numbers from lon.
(define (first-row n lon)
  (cond
    [(or (zero? n) (empty? lon))
     '()]
    [else
     (cons (first lon) (first-row (sub1 n) (rest lon)))]))

(check-expect (first-row 100 '())
              '())
(check-expect (first-row 0 (list 1 2 3 4 5 6 7 8 9 10))
              '())
(check-expect (first-row 1 (list 1 2 3 4 5 6 7 8 9 10))
              (list 1))
(check-expect (first-row 7 (list 1 2 3 4 5 6 7 8 9 10))
              (list 1 2 3 4 5 6 7))

; N [List-of Number] -> [List-of Number]
; Remove the first n numbers from lon.
; If n is greater than the length of lon,
; an empty list is returned.
(define (remove-first-row n lon)
  (cond
    [(empty? lon) '()]
    [(zero? n) lon]
    [else (remove-first-row (sub1 n) (rest lon))]))

(check-expect (remove-first-row 0 (list 1 2 3 4 5 6 7 8 9 10))
              (list 1 2 3 4 5 6 7 8 9 10))
(check-expect (remove-first-row 100 '())
              '())
(check-expect (remove-first-row 6 (list 1 2 3 4 5 6 7 8 9 10))
              (list 7 8 9 10))
(check-expect (remove-first-row 55 (list 1 2 3 4 5 6 7 8 9 10))
              '())
