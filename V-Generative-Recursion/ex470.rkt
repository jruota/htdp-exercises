;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex470) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

(define M1 (list (list 2 2  3 10)
                 (list 2 5 12 31)
                 (list 4 1 -2  1)))

(define S1 (list 1 1 2))

(define M2 (list (list 2  3  3 8)
                 (list 2  3 -2 3)
                 (list 4 -2  2 4)))

(define S2 (list 1 1 1))

(define M3 (list (list 2 2 2 6)
                 (list 2 2 4 8)
                 (list 2 2 1 2)))

(define LENGTH-ERROR "equations must have the same length")
(define SOLUTION-ERROR "the system of equations has no solution")

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; SOE -> Solution
; Return the solution of soe if it
; has a solution. Raise an error otherwise.
(define (gauss soe)
  (solve (triangulate soe)))

(check-expect (gauss M1) S1)
(check-expect (gauss M2) S2)
(check-error (gauss M3) SOLUTION-ERROR)

; from ex469.rkt ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

; from ex468.rkt ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; SOE -> TM
; Triangulate the given system of equations.
(define (triangulate M)
  (cond
    [(= (length M) 1) M]
    [(all-leading-coefficients-zero? M) (error SOLUTION-ERROR)]
    [else
     (local (; SOE -> SOE
             ; Subtract a multiple of top from the equations in m,
             ; item by item, so that the resulting equations have a 0 in
             ; the first position, i.e. the first coeffiecient is 0.
             (define (subtract-loop top m)
               (cond
                 [(empty? m) '()]
                 [else
                  (cons (subtract (first m) top)
                        (subtract-loop top (rest m)))])))
       ; – IN –
       (cons (first M)
             (triangulate
              (rotate
               (subtract-loop (first M) (rest M))))))]))

; SOE -> Boolean
; Are all leading coefficients
; in soe zero?
(define (all-leading-coefficients-zero? soe)
  (cond
    [(empty? soe) #true]
    [else
     (and (zero? (first (first soe)))
          (all-leading-coefficients-zero? (rest soe)))]))

; from ex467.rkt ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; SOE -> SOE
; Switch equations in soe so that those with
; a leading coefficient of zero come last.
(define (rotate soe)
  (cond
    [(empty? soe) '()]
    [else
     (if (zero? (first (first soe)))
         (append (rotate (rest soe)) (list (first soe)))
         (cons (first soe) (rotate (rest soe))))]))

; from ex466.rkt ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Equation Equation -> Equation
; Subtract a multiple of the second equation from the first,
; item by item, so that the resulting Equation has a 0 in
; the first position.
(define (subtract e1 e2)
  (cond
    [(= (length e1) (length e2))
     (local ((define coeff (/ (first e1) (first e2))))
       ; – IN –
       ; (rest ...) removes the leading zero
       (rest (map (lambda (x y) (- x (* coeff y))) e1 e2)))]
    [else
     (error LENGTH-ERROR)]))

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

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Equation -> [List-of Number]
; extracts the left-hand side from a row in a matrix
(define (lhs e)
  (reverse (rest (reverse e))))
 
; Equation -> Number
; extracts the right-hand side from a row in a matrix
(define (rhs e)
  (first (reverse e)))