;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex465) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; An SOE is a non-empty Matrix.
; constraint for (list r1 ... rn), (length ri) is (+ n 1)
; interpretation represents a system of linear equations
 
; An Equation is a [List-of Number].
; constraint an Equation contains at least two numbers. 
; interpretation if (list a1 ... an b) is an Equation, 
; a1, ..., an are the left-hand-side variable coefficients 
; and b is the right-hand side
 
; A Solution is a [List-of Number]

; A Matrix is one of: 
;  – (cons Row '())
;  – (cons Row Matrix)
; constraint all rows in matrix are of the same length
 
; A Row is one of: 
;  – '() 
;  – (cons Number Row)

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define M ; an SOE 
  (list (list 2 2  3 10) ; an Equation 
        (list 2 5 12 31)
        (list 4 1 -2  1)))

(define M2 (list (list 2 2  3 10)
                 (list 0 3  9 21)
                 (list 0 0  1  2)))

(define M3 (list (list 2  2  3   10)
                 (list 0  3  9   21)
                 (list 0 -3 -8  -19)))
 
(define S '(1 1 2)) ; a Solution

(define LENGTH-ERROR "equations must have the same length")

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Equation Equation -> Equation
; Subtract a multiple of the second equation from the first,
; item by item, so that the resulting Equation has a 0 in
; the first position.
(define (subtract e1 e2)
  (cond
    [(= (length e1) (length e2))
     (local ((define coeff (/ (first e1) (first e2))))
       ; – IN –
       (map (lambda (x y) (- x (* coeff y))) e1 e2))]
    [else
     (error LENGTH-ERROR)]))

(check-error (subtract (list 1 2 3) (list 1))
             LENGTH-ERROR)

(check-expect (subtract (lhs (second M)) (lhs (first M)))
              (lhs (second M2)))
(check-expect (subtract (lhs (third M)) (lhs (first M)))
              (lhs (third M3)))

; Equation Equation -> Equation
; Subtract a multiple of the second equation from the first,
; item by item, so that the resulting Equation has a 0 in
; the first position.
(define (subtract.v2 e1 e2)
  (cond
    [(= (length e1) (length e2))
     (local ((define coeff (/ (first e1) (first e2))))
       ; – IN –
       ; (rest ...) removes the leading zero
       (rest (map (lambda (x y) (- x (* coeff y))) e1 e2)))]
    [else
     (error LENGTH-ERROR)]))

(check-error (subtract.v2 (list 1 2 3) (list 1))
             LENGTH-ERROR)

(check-expect (subtract.v2 (lhs (second M)) (lhs (first M)))
              (list 3  9))
(check-expect (subtract.v2 (lhs (third M)) (lhs (first M)))
              (list -3 -8))

; Equation -> [List-of Number]
; extracts the left-hand side from a row in a matrix
(check-expect (lhs (first M)) '(2 2 3))
(define (lhs e)
  (reverse (rest (reverse e))))
 
; Equation -> Number
; extracts the right-hand side from a row in a matrix
(check-expect (rhs (first M)) 10)
(define (rhs e)
  (first (reverse e)))
