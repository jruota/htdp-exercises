;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex490) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [List-of Number] -> [List-of Number]
; converts a list of relative to absolute distances
; the first number represents the distance to the origin
(define (relative->absolute l)
  (cond
    [(empty? l) '()]
    [else (local ((define rest-of-l
                    (relative->absolute (rest l)))
                  (define adjusted
                    (add-to-each (first l) rest-of-l)))
            ; – IN –
            (cons (first l) adjusted))]))

(check-expect (relative->absolute '()) '())
(check-expect (relative->absolute '(50 40 70 30 30))
              '(50 90 160 190 220))

; Number [List-of Number] -> [List-of Number]
; adds n to each number on l
(define (add-to-each n l)
;  (cond
;    [(empty? l) '()]
;    [else
;     (cons (+ n (first l))
;           (add-to-each n (rest l)))]))
  (map (lambda (x) (+ x n)) l))

(check-expect (add-to-each 10 '()) '())
(check-expect (add-to-each 0 (list 1 2 3 4 5))
              (list 1 2 3 4 5))
(check-expect (add-to-each 10 (list 1 2 3 4 5))
              (list 11 12 13 14 15))
(check-expect (cons 50 (add-to-each 50 '(40 110 140 170)))
              '(50 90 160 190 220))

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(build-list 1 add1)
(build-list 2 add1)
(build-list 3 add1)

; Both relative->absolute and add-to-each use on the order of n steps where n is
; the number of items in a list. Every call to relative->absolute with a list of
; length n sets off n calls to add-to-each.
; To get the total amount of steps needed one needs to add the steps for
; relative->absolute (n) to the total steps for add-to-each (* 1/2 n (+ n 1)).
; Then the amount for the total steps can be calculated by the following
; formula:
;
;     (+ n (* 1/2 n (+ n 1))) = (* 1/2 (+ (* n n) (* 3 n)))
;
; So, relative->absolute uses on the order of (* n n) [n-squared] steps, where n
; is the number of items in a list.

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; length of list    recursive calls to       calls to           
;     n             relative->absolute       add-to-each        total calls
; ––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
;     0             0                         0                  0
;     1             1                         1                  2
;     2             2                         3                  5
;     3             3                         6                  9
;     4             4                        10                 14
;     5             5                        15                 20
;    ...           ...                       ...                ...
