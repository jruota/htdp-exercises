;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex139) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A List-of-numbers is one of: 
; – '()
; – (cons Number List-of-numbers)

;         |---------------------
;        \|/                   |
; A List-of-amounts is one of: |
; – '()                        |
; – (cons PositiveNumber List-of-amounts)
; Interpretation:
;     A list of amounts of money.

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; NOTE - - - - - - - - - - - - - - - - -
; The result of (positive? 0) is #false.
; 0 is neither positive nor negative.

; The function hast to return #true
; for the empty list, otherwise
; the recursive calls for lists
; with one positive element would not
; work.
; END NOTE - - - - - - - - - - - - - - -

; Determine whether all elements of
; lon are positive.
(define (pos? lon)
  (cond
    [(empty? lon) #true]
    [(cons? lon)
     (and (positive? (first lon))
          (pos? (rest lon)))]))

(check-expect (pos? '())
              #true)
(check-expect (pos? (cons 5 '()))
              #true)
(check-expect (pos? (cons -1 '()))
              #false)
(check-expect (pos? (cons 10
                          (cons 324
                                (cons 6
                                      (cons 17
                                            (cons 88
                                                  (cons 0 '())))))))
              #false)
(check-expect (pos? (cons 10
                          (cons 324
                                (cons 6
                                      (cons 17
                                            (cons 88
                                                  (cons -15 '())))))))
              #false)
(check-expect (pos? (cons 10
                          (cons 324
                                (cons 6
                                      (cons 17
                                            (cons 88
                                                  (cons 1 '())))))))
              #true)

; List-of-amounts -> Number
; Compute the sum of the amounts
; in loa if loa is an element of
; List-of-amounts.
(define (checked-sum loa)
  (if (pos? loa)
      (sum loa)
      (error "Only positive amounts allowed.")))

(check-error (checked-sum (cons 10 (cons 9 (cons 8 (cons -1 '())))))
             "Only positive amounts allowed.")
(check-expect (checked-sum (cons 1 (cons 2 (cons 3 (cons 444 '())))))
              450)

; List-of-amounts -> Number
; Compute the sum of the amounts
; in loa.
(define (sum loa)
  (cond
    [(empty? loa) 0]
    [(cons? loa)
     (+ (first loa)
        (sum (rest loa)))]))

(check-expect (sum '())
              0)
(check-expect (sum (cons 1 '()))
              1)
(check-expect (sum (cons 1 (cons 2 '())))
              3)
(check-expect (sum (cons 1 (cons 2 (cons 3 '()))))
              6)
(check-expect (sum (cons 1 (cons 2 (cons 3 (cons 444 '())))))
              450)

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; What does sum compute for an element of List-of-numbers?

; When all elements are positve, sum will calculate the sum
; of all amounts as stated in the purpose statement. If any
; element is negative though, sum will subtract it from the
; total.