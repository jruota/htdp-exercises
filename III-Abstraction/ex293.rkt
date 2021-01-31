;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex293) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A [Maybe X] is one of: 
; – #false 
; – X

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; X [List-of X] -> [Maybe [List-of X]]
; returns the first sublist of l that starts
; with x, #false otherwise
(define (find x l)
  (cond
    [(empty? l) #false]
    [else
     (if (equal? (first l) x) l (find x (rest l)))]))

(check-satisfied (find 5 '())
                 (found? 5 '()))
(check-satisfied (find 5 (list 1 2 3 4 5 6 7 8 9))
                 (found? 5 (list 1 2 3 4 5 6 7 8 9)))
(check-satisfied (find 5 (list 1 2 5 3 4 5 6 7 8 9))
                 (found? 5 (list 1 2 5 3 4 5 6 7 8 9)))

; X [List-of X] -> [[List-of X] -> Boolean]
; Is l0 the first sublist of l that starts
; with x?
(define (found? x l)
  (lambda (l0)
    (cond
      [(boolean? l0)
       (not l0)]
      [else
       (and (equal-or-tail? l0 l)
            (equal? (first l0) x)
            (= (count x l0) (count x l)))])))

; NOTE: is this really how it should be?
(check-expect ((found? 1 '()) #false)
              #true)
(check-expect ((found? 1 '()) #true)
              #false)

(check-expect ((found? 1 (list 1 2 3 4 5 6 7 8 9)) (list 1 2 3 4 5 6 7 8 9))
              #true)
(check-expect ((found? 5 (list 1 2 3 4 5 6 7 8 9)) (list 5 6 7 8 9))
              #true)
(check-expect ((found? 9 (list 1 2 3 4 5 6 7 8 9)) (list 9))
              #true)

(check-expect ((found? 5 (list 1 2 5 3 4 5 6 7 8 9)) (list 5 3 4 5 6 7 8 9))
              #true)

(check-expect ((found? 10 (list 1 2 3 4 5 6 7 8 9)) #false)
              #true)

(check-expect ((found? 5 (list 1 2 5 3 4 5 6 7 8 9)) (list 5 6 7 8 9))
              #false)

; [List-of X] [List-of X] -> Boolean
; Is l1 equal to l2 or
; is l1 a tail of l2?
(define (equal-or-tail? l1 l2)
  (cond
    [(empty? l2)
     (empty? l1)]
    [(cons? l2)
     (or (equal? l1 l2)
         (equal-or-tail? l1 (rest l2)))]))

(check-expect (equal-or-tail? '() (list 1 2 3 4 5 6 7 8 9))
              #true)
(check-expect (equal-or-tail? (list 6 7 8 9) (list 1 2 3 4 5 6 7 8 9))
              #true)
(check-expect (equal-or-tail? (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9))
              #true)
(check-expect (equal-or-tail? (list 6 7 8) (list 1 2 3 4 5 6 7 8 9))
              #false)
     
; X [List-of X] -> Number
; Count how often x occurs in l.
(define (count x l)
  (cond
    [(empty? l) 0]
    [(cons? l)
     (if (equal? (first l) x)
         (+ 1 (count x (rest l)))
         (count x (rest l)))]))

(check-expect (count 5 '())
              0)
(check-expect (count 5 (list 1 2 3 4 5 6 7 8 9))
              1)
(check-expect (count 5 (list 1 2 5 3 4 5 6 7 8 5 9))
              3)