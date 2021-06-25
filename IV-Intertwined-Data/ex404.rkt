;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex404) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; [X Y -> Boolean] [List-of X] [List-of Y] -> Boolean
; Return #true if (f x_i y_i) yields true for all
; corresponding pairs from the lists of equal length
; l1 and l2. Return #false otherwise.
(define (andmap2 f l1 l2)
  (cond
    [(empty? l1) #true]
    [else
     (and (f (first l1) (first l2))
          (andmap2 f (rest l1) (rest l2)))]))

(check-expect (andmap2 > '() '())
              #true)
(check-expect (andmap2 <
                       (list 1 2 3 4 5)
                       (list 2 3 4 5 6))
              #true)
(check-expect (andmap2 =
                       (list 1 2 3 4 5)
                       (list 1 2 3 5 4))
              #false)