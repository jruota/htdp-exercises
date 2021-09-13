;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex469) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; An SOE is a non-empty Matrix.
; constraint for (list r1 ... rn), (length ri) is (+ n 1)
; interpretation represents a system of linear equations
 
; An Equation is a [List-of Number].
; constraint an Equation contains at least two numbers. 
; Interpretation: if (list a1 ... an b) is an Equation,
;                 a1, ..., an are the left-hand-side variable coefficients
;                 and b is the right-hand side
 
; A Solution is a [List-of Number]

; A TM is an [NEList-of Equation]
; such that the Equations are of decreasing length: 
;   n + 1, n, n - 1, ..., 2. 
; Interpretation: represents a triangular matrix.

; A Matrix is one of: 
;  – (cons Row '())
;  – (cons Row Matrix)
; Constraint: all rows in Matrix are of the same length.
 
; A Row is one of: 
;  – '() 
;  – (cons Number Row)

; An N is one of:
; – 0
; – (add1 N)
; Interpretation: The natural numbers.

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define M ; an SOE 
  (list (list 2 2  3 10) ; an Equation 
        (list 2 5 12 31)
        (list 4 1 -2  1)))

(define M1 (list (list 2 2  3 10)
                 (list 3 9 21)
                 (list 1 2)))
 
(define S1 (list 1 1 2))

(define M2 (list (list 2  3  3   8)
                 (list   -8 -4 -12)
                 (list      -5  -5)))

(define S2 (list 1 1 1))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; TM -> [List-of Number]
; Find the solution of the triangular
; matrix tm.
(define (solve tm)
  (local (; Equation [List-of Number]
          ; Solve for the first coefficient in eq
          ; by replacing all other coefficients with
          ; the values from lon, i.e. solve the
          ; equation eq with (+ n 1) variables
          ; given a solution for the last n variables.
          (define (solve-equation eq lon)
            (local ((define first-coefficient (first eq))
                    (define solved-coefficients (rest (lhs eq)))
                    (define right-hand-side (rhs eq)))
              ; – IN –
              (/ (- right-hand-side (plug-in solved-coefficients lon))
                 first-coefficient))))
    ; – IN –
    (foldr (lambda (x y) (cons (solve-equation x y) y)) '() tm)))

(check-expect (solve M1) S1)
(check-expect (solve M2) S2)

; from ex462.rkt ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; [List-of Number] Solution -> Number
; Calculate the value of the left-hand side when the numbers from sol
; are plugged in for the variables in l.
(define (plug-in l sol)
  (cond
    [(or (empty? l) (empty? sol)) 0]
    [else
     (+ (* (first l) (first sol))
        (plug-in (rest l) (rest sol)))]))

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
