;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex291) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A [List-of ITEM] is one of:
;     – '()
;     – (cons ITEM [List-of ITEM])
; Interpretation:
;     A list of ITEMs.

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; NOTE +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

; Not a true "map" clone, as it can handle only one list where "map" can handle
; an arbitrary number of lists.

; END NOTE +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

; [X Y] [X -> Y] [List-of X] -> [List-of Y]
; Return a list containing the results
; of applying proc to the elements of lst
; in order.
(define (own-map proc lst)
    (foldr
     (lambda (x y)
       (cons (proc x) y))
     '() lst))

(check-expect (own-map add1 '())
              '())
(check-expect (own-map add1 (list 1 2 3 4 5))
              (list 2 3 4 5 6))
(check-expect (own-map string-upcase (list "apple" "banana" "orange"))
              (list "APPLE" "BANANA" "ORANGE"))