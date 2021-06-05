;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex394) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; [List-of Number] [List-of Number] -> [List-of Number]
; The function consumes two lists of numbers, sorted in ascending order.
; It produces a single sorted list of numbers that contains all the numbers
; on both inputs lists. A number occurs in the output as many times as it
; occurs on the two input lists together.
(define (merge lon1 lon2)
  (cond
    [(empty? lon1) lon2]
    [(empty? lon2) lon1]
    [(cons? lon2)
     (if (<= (first lon1) (first lon2))
         (cons (first lon1) (merge (rest lon1) lon2))
         (cons (first lon2) (merge lon1 (rest lon2))))]))

(check-expect (merge '() '())
              '())
(check-expect (merge (list 1 2 3 4 5 6 7) '())
              (list 1 2 3 4 5 6 7))
(check-expect (merge '() (list 4 5 6 7 8 9 10 11))
              (list 4 5 6 7 8 9 10 11))
(check-expect (merge (list 1 2 3 4 5 6 7) (list 4 5 6 7 8 9 10 11))
              (list 1 2 3 4 4 5 5 6 6 7 7 8 9 10 11))