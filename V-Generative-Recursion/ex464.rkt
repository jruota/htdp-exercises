;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex464) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

(define ARG-ERROR "arguments must have the same length")

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; SOE Solution -> Boolean
; Return #true if plugging in the numbers from sol
; for the variables in the Equations of soe produces
; equal left-hand-side values and right-hand-side values
; Return #false otherwise.
(define (check-solution soe sol)
  (cond
    [(empty? soe) #true]
    [else
     (and (= (plug-in (lhs (first soe)) sol) (rhs (first soe)))
          (check-solution (rest soe) sol))]))

(check-expect (check-solution M S)
              #true)
(check-expect (check-solution M (list 1 1 1))
              #false)

(check-satisfied (check-solution M S) (lambda (x) (not (false? x))))
(check-satisfied (check-solution M (list 1 1 1)) false?)

; [List-of Number] Solution -> Number
; Calculate the value of the left-hand side when the numbers from sol
; are plugged in for the variables in l.
(define (plug-in l sol)
  (local (; [List-of Number] Solution -> Number
          ; Do the actual work.
          (define (main l0 sol0)
            (cond
              [(and (empty? l0) (empty? sol0)) 0]
              [else
               (+ (* (first l0) (first sol0))
                  (plug-in (rest l0) (rest sol0)))])))
    ; – IN –
    (cond
      [(= (length l) (length sol))
       (main l sol)]
      [else
       (error ARG-ERROR)])))

(check-expect (plug-in (lhs (first M)) S)
              10)
(check-expect (plug-in (lhs (second M)) S)
              31)
(check-expect (plug-in (lhs (third M)) S)
              1)

(check-error (plug-in '() S)
             ARG-ERROR)
(check-error (plug-in (lhs (first M)) '())
             ARG-ERROR)

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

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(check-solution M2 S)
(check-solution M3 S)
