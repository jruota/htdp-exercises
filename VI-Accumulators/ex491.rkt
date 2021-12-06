;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex491) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [List-of Number] -> [List-of Number]
; Convert a list of relative to absolute distances.
; The first number represents the distance to the origin.
(define (relative->absolute l)
 (reverse
   (foldr (lambda (x y) (cons (+ x (first y)) y))
          (list (first l))
          (reverse (rest l)))))

; does not work for empty lists
;(check-expect (relative->absolute '()) '())
(check-expect (relative->absolute '(50 40 70 30 30))
              '(50 90 160 190 220))

; [List-of X] -> [List-of X]
; Return a list that has the same elements as lst,
; but in reverse order.
(define (reverse-own lst)
  (cond
    [(empty? lst) '()]
    [else
     (insert-at-the-end
      (first lst)
      (reverse-own (rest lst)))]))

(check-expect (reverse-own '()) '())
(check-expect (reverse-own (list 1 2 3 4 5))
              (list 5 4 3 2 1))

; X [List-of Y] -> [List-of Z]
; Insert item at the end of lst.
(define (insert-at-the-end item lst)
  (cond
    [(empty? lst)
     (cons item '())]
    [else
     (cons (first lst)
           (insert-at-the-end item (rest lst)))]))

(check-expect (insert-at-the-end 74 '())
              (list 74))
(check-expect (insert-at-the-end 37 (list 1 2 3 4 5))
              (list 1 2 3 4 5 37))

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Does your friend’s solution mean there is no need for our complicated design
; in this motivational section?

; The version with the accumulator uses on the order of n steps where n is the
; number of items in a list.
; This version uses reverse, and if it is similar to reverse-own, reverse uses
; on the order of n^2 [i.e. (* n n)] steps where n is the number of items in the
; list. This would incur a hefty performance deficit compared to the version
; using an accumulator.
