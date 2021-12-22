;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex467-alt) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

(define M2 (list (list 2 2  3 10)
                 (list 3 9 21)
                 (list 1 2)))

(define M3 (list (list  2  2  3   10)
                 (list     3  9   21)
                 (list    -3 -8  -19)))
 
(define S '(1 1 2)) ; a Solution

(define M4 (list (list 2  3  3 8)
                 (list 2  3 -2 3)
                 (list 4 -2  2 4)))

(define M5 (list (list 2  3  3   8)
                 (list   -8 -4 -12)
                 (list      -5  -5)))

(define SolForM4 (list 1 1 1))

(define LENGTH-ERROR "equations must have the same length")
(define LENGTH-ERROR2 "equation is too short")
(define LENGTH-ERROR3 "list is too short")
(define EMPTY-ERROR "the system of equations must be a non-empty Matrix")

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Does this algorithm terminate for all possible system of equations?

    ; No, the subtract function will throw a division by zero error when all
    ; rows start with 0. The rotate function will loop forever.

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; SOE -> TM
; Triangulate the given system of equations.
(define (triangulate M)
  (cond
    [(= (length M) 1) M]
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
               ;(rotate-alt
                (subtract-loop (first M) (rest M))))))]))

(check-expect (triangulate (list (list 1 2 3) (list 2 3 4)))
              (list (list 1 2 3) (list -1 -2))) 
; The following test does not make sense here, since it violates the
; Matrix definition.
;(check-expect (triangulate M2) M2)
(check-expect (triangulate M) M2)
(check-expect (triangulate M4) M5)

; SOE -> SOE
; Find an equation with a leading coefficient that is not 0
; and place it as the first row in soe.
(define (rotate soe)
;  (cond
;    [(empty? soe) '()]
;    [else
;     (if (zero? (first (first soe)))
;         (append (rotate (rest soe)) (list (first soe)))
;         (cons (first soe) (rotate (rest soe))))]))
  (cond
    [(zero? (first (first soe)))
     (rotate (append (rest soe) (list (first soe))))]
    [else soe]))

; NOTE ------------------------------------
; An SOE is defined as an non-empty Matrix.
; END NOTE --------------------------------
;(check-expect (rotate '()) '())
(check-expect (rotate (list (list 0 1 2 3 4 5)
                            (list 0 2 3 4 5 6)
                            (list 0 3 4 5 6 7)
                            (list 0 4 5 6 7 8)
                            (list 1 2 3 4 5 6)
                            (list 0 5 6 7 8 9)))
              (list (list 1 2 3 4 5 6)
                    (list 0 5 6 7 8 9)
                    (list 0 1 2 3 4 5)
                    (list 0 2 3 4 5 6)
                    (list 0 3 4 5 6 7)
                    (list 0 4 5 6 7 8)))
(check-expect (rotate M5) M5)

; SOE -> SOE
; Switch equations in soe so that those with
; a leading coefficient of zero come last.
(define (rotate-alt soe)
  (cond
    [(empty? soe) '()]
    [else
     (local (; [List-of Number] -> N
             ; Count the number of leading
             ; zeroes in lst.
             (define (clz lst)    ; count leading zeroes
               (cond
                 [(empty? lst) 0]
                 [else
                  (if (zero? (first lst))
                      (+ 1 (clz (rest lst)))
                      0)])))
       ; – IN –
       (sort soe (lambda (x y) (< (clz x) (clz y)))))]))


(check-expect (rotate-alt '()) '())
(check-expect (rotate-alt (list (list 0 0 1 2 3 4)
                                (list 0 0 0 1 2 3)
                                (list 0 1 2 3 4 5)
                                (list 0 0 0 0 0 1)
                                (list 1 2 3 4 5 6)
                                (list 0 0 0 0 1 2)))
              (list (list 1 2 3 4 5 6)
                    (list 0 1 2 3 4 5)
                    (list 0 0 1 2 3 4)
                    (list 0 0 0 1 2 3)
                    (list 0 0 0 0 1 2)
                    (list 0 0 0 0 0 1)))
(check-expect (rotate-alt M5) M5)

; from ex465.rkt ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

(check-error (subtract (list 1 2 3) (list 1))
             LENGTH-ERROR)
(check-expect (subtract (lhs (second M)) (lhs (first M)))
              (lhs (second M2)))
(check-expect (subtract (lhs (third M)) (lhs (first M)))
              (lhs (third M3)))

; from ex462.rkt ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

; [List-of Number] Solution -> Number
; Calculate the value of the left-hand side when the numbers from sol
; are plugged in for the variables in l.
(define (plug-in l sol)
  (cond
    [(or (empty? l) (empty? sol)) 0]
    [else
     (+ (* (first l) (first sol))
        (plug-in (rest l) (rest sol)))]))

(check-expect (plug-in '() S)
              0)
(check-expect (plug-in (lhs (first M)) '())
              0)
(check-expect (plug-in (lhs (first M)) S)
              10)
(check-expect (plug-in (lhs (second M)) S)
              31)
(check-expect (plug-in (lhs (third M)) S)
              1)

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

(check-solution M4 SolForM4)
(check-solution M5 SolForM4)