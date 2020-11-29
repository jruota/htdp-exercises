;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex251) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; [List-of Number] -> Number
; computes the sum of 
; the numbers on l
(define (sum l)
  (cond
    [(empty? l) 0]
    [else
     (+ (first l)
        (sum (rest l)))]))

(check-expect (sum (list))
              0)
(check-expect (sum (list 0 1 2 3 4 5 6 7 8 9))
              45)

; [List-of Number] -> Number
; computes the product of 
; the numbers on l
(define (product l)
  (cond
    [(empty? l) 1]
    [else
     (* (first l)
        (product (rest l)))]))

(check-expect (product (list))
              1)
(check-expect (product (list 1 2 3 4 5 6 7 8 9))
              362880)

; [List-of Number] [Number Number -> Number] -> Number
; Compute a number by applying
; f to every element in l.
(define (fold1 l f)
  (cond
    [(empty? l) (f)]
    [else
     (f (first l)
        (fold1 (rest l) f))]))

(check-expect (fold1 (list) +)
              0)
(check-expect (fold1 (list 0 1 2 3 4 5 6 7 8 9) +)
              45)

(check-expect (fold1 (list) *)
              1)
(check-expect (fold1 (list  1 2 3 4 5 6 7 8 9) *)
              362880)
