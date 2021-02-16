;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex315) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)

; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define-struct no-parent [])
(define-struct child [father mother name date eyes])
(define NP (make-no-parent))
; An FT (short for family tree) is one of: 
; – NP
; – (make-child FT FT String N String)

; A [List-of X] is one of:
; – '()
; – (cons X [List-of X])

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

; Family Forests
(define ff1 (list Carl Bettina))
(define ff2 (list Fred Eva))
(define ff3 (list Fred Eva Carl))
(define ff4 (list Gustav))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; [List-of FT] N -> Number
; Produce the average age of all
; child instances in the forest.
(define (average-age ff y)
  (cond
    [(empty? ff) 0]
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
                     (total-age (child-mother ft)))]))

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

             (define cumulative-age (for/sum ([ft ff]) (total-age ft)))
             (define number-of-children (for/sum ([ft ff]) (count-persons ft))))
       ; IN
       (if (zero? number-of-children)
           0
           (/ cumulative-age number-of-children)))]))

(check-expect (average-age '() 2021)
              0)
(check-expect (average-age (list (make-no-parent)
                                 (make-no-parent)
                                 (make-no-parent))
                           2021)
              0)
(check-within (average-age (list Adam Dave Gustav) 2021)
              77.36
              .01)