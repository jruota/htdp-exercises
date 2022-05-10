;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex311) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define-struct no-parent [])
(define-struct child [father mother name date eyes])
(define NP (make-no-parent))
; An FT is one of: 
; – NP
; – (make-child FT FT String N String)

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Oldest Generation:
(define Carl (make-child NP NP "Carl" 1926 "green"))
(define Bettina (make-child NP NP "Bettina" 1926 "green"))
 
; Middle Generation:
(define Adam (make-child Carl Bettina "Adam" 1950 "hazel"))
(define Dave (make-child Carl Bettina "Dave" 1955 "black"))
(define Eva (make-child Carl Bettina "Eva" 1965 "blue"))
(define Fred (make-child NP NP "Fred" 1966 "pink"))
 
; Youngest Generation: 
(define Gustav (make-child Fred Eva "Gustav" 1988 "brown"))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; FT Number -> Number
; Given a family tree and the current year,
; produce the average age of all child structures
; in the family tree.
(define (average-age ft y)
  (cond
    [(no-parent? ft) 0]
    [else
     (/ (sum-of-ages ft y) (count-persons ft))]))

(define YEAR 2022)
(check-expect (average-age NP YEAR) 0)
(check-expect (average-age Carl YEAR) (- YEAR (child-date Carl)))
(check-expect (average-age Bettina YEAR) (- YEAR (child-date Bettina)))
(check-expect (average-age Adam YEAR) (/ (+ (- YEAR (child-date Adam))
                                            (- YEAR (child-date Carl))
                                            (- YEAR (child-date Bettina)))
                                         3))
(check-expect (average-age Dave YEAR) (/ (+ (- YEAR (child-date Dave))
                                            (- YEAR (child-date Carl))
                                            (- YEAR (child-date Bettina)))
                                         3))
(check-expect (average-age Eva YEAR) (/ (+ (- YEAR (child-date Eva))
                                           (- YEAR (child-date Carl))
                                           (- YEAR (child-date Bettina)))
                                        3))
(check-expect (average-age Fred YEAR) (- YEAR (child-date Fred)))
(check-expect (average-age Gustav YEAR) (/ (+ (- YEAR (child-date Gustav))
                                              (- YEAR (child-date Fred))
                                              (- YEAR (child-date Eva))
                                              (- YEAR (child-date Bettina))
                                              (- YEAR (child-date Carl)))
                                           5))

; FT Number -> Number
; Given a family tree and the current year,
; produce the sum of the ages of all child structures
; in the family tree.
(define (sum-of-ages ft y)
  (local (; FT -> Number
          ; Helper function.
          (define (main ft0)
            (cond
              [(no-parent? ft0) 0]
              [else
               (+ (- y (child-date ft0))
                  (main (child-father ft0))
                  (main (child-mother ft0)))])))
    ; – IN –
    (main ft)))

(check-expect (sum-of-ages Carl YEAR) (- YEAR (child-date Carl)))
(check-expect (sum-of-ages Bettina YEAR) (- YEAR (child-date Bettina)))
(check-expect (sum-of-ages Adam YEAR) (+ (- YEAR (child-date Adam))
                                         (- YEAR (child-date Carl))
                                         (- YEAR (child-date Bettina))))
(check-expect (sum-of-ages Dave YEAR) (+ (- YEAR (child-date Dave))
                                         (- YEAR (child-date Carl))
                                         (- YEAR (child-date Bettina))))
(check-expect (sum-of-ages Eva YEAR) (+ (- YEAR (child-date Eva))
                                        (- YEAR (child-date Carl))
                                        (- YEAR (child-date Bettina))))
(check-expect (sum-of-ages Fred YEAR) (- YEAR (child-date Fred)))
(check-expect (sum-of-ages Gustav YEAR) (+ (- YEAR (child-date Gustav))
                                           (- YEAR (child-date Carl))
                                           (- YEAR (child-date Fred))
                                           (- YEAR (child-date Eva))
                                           (- YEAR (child-date Bettina))))

; from ex310.rkt ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; FT -> Number
; Count the child structures in the tree.
(define (count-persons ft)
  (cond
    [(no-parent? ft) 0]
    [else
     (+ 1
        (count-persons (child-father ft))
        (count-persons (child-mother ft)))]))

(check-expect (count-persons NP) 0)
(check-expect (count-persons Carl) 1)
(check-expect (count-persons Bettina) 1)
(check-expect (count-persons Adam) 3)
(check-expect (count-persons Dave) 3)
(check-expect (count-persons Eva) 3)
(check-expect (count-persons Fred) 1)
(check-expect (count-persons Gustav) 5)
