;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex186) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A List-of-numbers is one of: 
; – '()
; – (cons Number List-of-numbers)

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; List-of-numbers -> List-of-numbers
; produces a sorted version of l
(define (sort> l)
  (cond
    [(empty? l) '()]
    [(cons? l) (insert (first l) (sort> (rest l)))]))

(check-expect (sort> '())
              '())
(check-expect (sort> (list 3 2 1))
              (list 3 2 1))
(check-expect (sort> (list 1 2 3))
              (list 3 2 1))
(check-expect (sort> (list 12 20 -5))
              (list 20 12 -5))

(check-satisfied (sort> '())
                 sorted>?)
(check-satisfied (sort> (list 3 2 1))
                 sorted>?)
(check-satisfied (sort> (list 1 2 3))
                 sorted>?)
(check-satisfied (sort> (list 12 20 -5))
                 sorted>?)
 
; Number List-of-numbers -> List-of-numbers
; inserts n into the sorted list of numbers l 
(define (insert n l)
  (cond
    [(empty? l) (cons n '())]
    [else (if (>= n (first l))
              (cons n l)
              (cons (first l) (insert n (rest l))))]))

(check-expect (insert 5 '())
              (list 5))
(check-expect (insert 5 (list 6))
              (list 6 5))
(check-expect (insert 5 (list 4))
              (list 5 4))
(check-expect (insert 12 (list 20 -5))
              (list 20 12 -5))
(check-expect (insert 4 (list 3 2 1))
              (list 4 3 2 1))
(check-expect (insert 1 (list 3 2 1))
              (list 3 2 1 1))
(check-expect (insert 2 (list 3 2 1))
              (list 3 2 2 1))

; From ex145.rkt (exercise 145) with minor changes. - - - - - - - - - - - - - - 
; List-of-numbers -> Boolean
; Return #true if the alon is sorted
; in descending order, #false else.
(define (sorted>? alon)
  (cond
    [(or (empty? alon)
         (empty? (rest alon)))
     #true]
    [else
     (and (>= (first alon) (second alon))
          (sorted>? (rest alon)))]))

(check-expect (sorted>? '())
              #true)
(check-expect (sorted>? (list 1))
              #true)
(check-expect (sorted>? (list 5 4 3 2 1))
              #true)
(check-expect (sorted>? (list 5 4 3 1 2))
              #false)

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; The check-satisfied function takes an expression an a primitive as arguments.
; Any primitive takes only one argument. To show that sort>/bad is not a sorting
; function one would have to show that the result is sorted and contains all
; elements from its argument. Therefore any primitive passed to check-satisfied
; would have to be given two arguments, the result of sort>/bad and the argument
; to sort>/bad to check for completeness. Thus one cannot formulate a test with
; check-satisfied.

; List-of-numbers -> List-of-numbers
; produces a sorted version of l
(define (sort>/bad l)
  (list 9 8 7 6 5 4 3 2 1 0))

; will result in #false
(check-expect (sorted>+complete? (list 3 8 6 7 1 2 9 0 -44)
                                 (sort>/bad (list 3 8 6 7 1 2 9 0 -44)))
              #true)

; List List -> Boolean
; Is new sorted in descending order
; and is every element of old in new?
(define (sorted>+complete? old new)
  (and (sorted>? new)
       (complete? old new)))

(check-expect (sorted>+complete? '() '())
              #true)
(check-expect (sorted>+complete? '() (list 5 4 3 2 1))
              #true)
(check-expect (sorted>+complete? (list 4 3 5 2 1) '())
              #false)

(check-expect (sorted>+complete? (list 3 1 5 2 4) (list 5 4 3 2 1))
              #true)
(check-expect (sorted>+complete? (list 3 1 5 2 4) (list 4 3 2 5 1))
              #false)
(check-expect (sorted>+complete? (list 3 1 5 2 4) (list 5 4 2 1))
              #false)

; List List -> Boolean
; Is every element of old in new?
; Does not check whether the number
; or appearances of any element is equal
; in both.
(define (complete? old new)
  (cond
    [(empty? old) #true]
    [(cons? old)
     (and (member? (first old) new)
          (complete? (rest old) new))]))

(check-expect (complete? '() '())
              #true)
(check-expect (complete? '() (list 1 2 3 4 5))
              #true)
(check-expect (complete? (list 1 2 3 4 5) '())
              #false)

(check-expect (complete? (list 1 4 7 3) (list 7 7 3 1 4 7 1))
              #true)
(check-expect (complete? (list 1 4 7 3) (list 7 7 1 4 7 1))
              #false)