;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex315) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define-struct no-parent [])
(define-struct child [father mother name date eyes])
(define NP (make-no-parent))
; An FT is one of: 
; – NP
; – (make-child FT FT String N String)

; An FF (short for family forest) is one of: 
; – '()
; – (cons FT FF)
; interpretation a family forest represents several
; families (say, a town) and their ancestor trees

; An FF (short for family forest) is a [List-of FT].
; Interpretation: A family forest represents several
;                 families and their ancestor trees.

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

; Forests
(define ff1 (list Carl Bettina))
(define ff2 (list Fred Eva))
(define ff3 (list Fred Eva Carl))

(define YEAR 2022)

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; [List-of FT] Number -> Number
; Given a family forest and a year, produce the average age
; of all child instances in the forest.
; Note: If the trees in this forest overlap, the result isn’t
;       a true average because some people contribute more than others.
(define (average-age loft year)
  (local (; [List-of FT] -> Number
          ; Given the family forest loft0 and year,
          ; calculate the sum of all ages in loft0.
          (define (sum-of-ages-in-forest loft0)
            (cond
              [(empty? loft0) 0]
              [else
               (+ (sum-of-ages (first loft0) year)
                  (sum-of-ages-in-forest (rest loft0)))]))

          ; [List-of FT] -> Number
          ; Given the family forest loft1,
          ; return the number of child structures in loft1.
          (define (count-people-in-forest loft1)
            (cond
              [(empty? loft1) 0]
              [else
               (+ (count-persons (first loft1))
                  (count-people-in-forest (rest loft1)))])))
    ; – IN –
    (cond
      [(empty? loft) 0]
      [else
       (/ (sum-of-ages-in-forest loft)
          (count-people-in-forest loft))])))

(check-expect (average-age '() YEAR) 0)
(check-expect (average-age ff1 YEAR) (/ (+ (- YEAR (child-date (first ff1)))
                                      (- YEAR (child-date (second ff1))))
                                   2))
(check-expect (average-age ff2 YEAR) (/ (+ (- YEAR (child-date Fred))
                                           (- YEAR (child-date Eva))
                                           (- YEAR (child-date Bettina))
                                           (- YEAR (child-date Carl)))
                                        4))
(check-expect (average-age ff3 YEAR) (/ (+ (- YEAR (child-date Fred))
                                           (- YEAR (child-date Eva))
                                           (- YEAR (child-date Bettina))
                                           (- YEAR (child-date Carl))
                                           (- YEAR (child-date Carl)))
                                        5))

; [List-of FT] Number -> Number
; Given a family forest and a year, produce the average age
; of all child instances in the forest.
; Note: If the trees in this forest overlap, the result isn’t
;       a true average because some people contribute more than others.
(define (average-age.v2 loft year)
  (local (; [List-of FT] Number Number -> (list Number Number)
          ; Given the family forest loft0 and year,
          ; return a list of length 2 with the sum
          ; of all ages in loft0 and the number of
          ; all child structures in loft0.
          ; The input total-ages and total-people are starting
          ; values for the calculation.
          (define (main loft0 total-ages total-people)
            (cond
              [(empty? loft0)
               (list total-ages total-people)]
              [else
               (local ((define family-tree (first loft0)))
                 ; – IN –
                 (main (rest loft0)
                       (+ total-ages (sum-of-ages family-tree year))
                       (+ total-people (count-persons family-tree))))])))
    ; – IN –
    (cond
      [(empty? loft) 0]
      [else
       (local ((define res (main loft 0 0)))
         ; – IN –
         (/ (first res) (second res)))])))

(check-expect (average-age.v2 '() YEAR) 0)
(check-expect (average-age.v2 ff1 YEAR) (/
                                         (+ (- YEAR (child-date (first ff1)))
                                            (- YEAR (child-date (second ff1))))
                                         2))
(check-expect (average-age.v2 ff2 YEAR) (/ (+ (- YEAR (child-date Fred))
                                              (- YEAR (child-date Eva))
                                              (- YEAR (child-date Bettina))
                                              (- YEAR (child-date Carl)))
                                        4))
(check-expect (average-age.v2 ff3 YEAR) (/ (+ (- YEAR (child-date Fred))
                                              (- YEAR (child-date Eva))
                                              (- YEAR (child-date Bettina))
                                              (- YEAR (child-date Carl))
                                              (- YEAR (child-date Carl)))
                                           5))

; from ex311.rkt ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
