;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex311) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define-struct no-parent [])
(define-struct child [father mother name date eyes])
(define NP (make-no-parent))
; An FT (short for family tree) is one of: 
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

; FT N -> Number
; Calculate the average age of all
; child structures in the family tree ft
; in the year y.
(define (average-age ft y)
  (cond
    [(no-parent? ft) 0]
    [else
     (local (; FT -> Number
             ; Calculate the total age of
             ; all child structures in the
             ; family tree ft.
             (define (total-age ft)
               (cond
                 [(no-parent? ft) 0]
                 [else
                  (+ (if (> (- y (child-date ft)) 0)
                         (- y (child-date ft))
                         0)
                     (total-age (child-father ft))
                     (total-age (child-mother ft)))])))
       ; – IN –
       (/ (total-age ft) (count-persons ft)))]))

(check-expect (average-age NP 2021)
              0)
(check-expect (average-age Carl 2021)
              95)
(check-expect (average-age Eva 2021)
              82)
(check-expect (average-age Gustav 2021)
              66.8)

; FT -> N
; Count the child structures in
; the family tree ft.
(define (count-persons ft)
  (cond
    [(no-parent? ft) 0]
    [else
     (+ 1
        (count-persons (child-father ft))
        (count-persons (child-mother ft)))]))

(check-expect (count-persons NP)
              0)
(check-expect (count-persons Carl)
              1)
(check-expect (count-persons Eva)
              3)
(check-expect (count-persons Gustav)
              5)