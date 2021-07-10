;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex442) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; An N is one of:
; – 0
; – (add1 N)
; Interpretation:
;     The natural numbers.

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define INPUT-ERROR "integer greater or equal 0 expected")
(define THRESHOLD 50)

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; N -> [List-of Numbers]
; Return a list of numbers that has n items
; which are in the range [0, n].
(define (create-tests n)
  (cond
    [(and (integer? n) (>= n 0))
     (cond
       [(zero? n) '()]
       [(positive? n)
        (cons (random (add1 n))
              (create-tests (sub1 n)))])]
    [else
     (error INPUT-ERROR)]))

(check-expect (create-tests 0)
              '())
(check-expect (length (create-tests 5))
              5)

(check-error (create-tests -3)
             INPUT-ERROR)
(check-error (create-tests 3.14)
             INPUT-ERROR)

; from ex426.rkt and ex428.rkt ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; [List-of Number] -> [List-of Number]
; produces a sorted version of alon
(define (quick-sort< alon)
  (cond
    [(empty? alon) '()]
    ; [(empty? (rest alon)) alon]
    [else
     (append (quick-sort< (smallers alon (first alon)))
             (build-list (count (first alon) alon)
                         (lambda (x) (first alon)))
             (quick-sort< (largers alon (first alon))))]))

(check-expect (quick-sort< '()) '())
(check-expect (quick-sort< (list 23)) (list 23))
(check-expect (quick-sort< (list 11 8 14 7)) (list 7 8 11 14))
(check-expect (quick-sort< (list 11 9 2 18 12 14 4 1))
              (list 1 2 4 9 11 12 14 18))

; [List-of Number] Number -> [List-of Number]
; Return all numbers from lon that are less
; than n.
(define (smallers lon n)
  (cond
    [(empty? lon) '()]
    [else
     (if (< (first lon) n)
         (cons (first lon) (smallers (rest lon) n))
         (smallers (rest lon) n))]))

(check-expect (smallers (list 11 8 14 7) 11)
              (list 8 7))
(check-expect (smallers (list 11 9 2 18 12 14 4 1) 11)
              (list 9 2 4 1))

; [List-of Number] Number -> [List-of Number]
; Return all numbers from lon that are greater
; than n.
(define (largers lon n)
  (cond
    [(empty? lon) '()]
    [else
     (if (> (first lon) n)
         (cons (first lon) (largers (rest lon) n))
         (largers (rest lon) n))]))

(check-expect (largers (list 11 8 14 7) 11)
              (list 14))
(check-expect (largers (list 11 9 2 18 12 14 4 1) 11)
              (list 18 12 14))

; [List-of X] X -> N
; Count how often x appears in lox.
(define (count x lox)
  (cond
    [(empty? lox) 0]
    [else
     (if (equal? x (first lox))
         (+ 1 (count x (rest lox)))
         (count x (rest lox)))]))

(check-expect (count 1 '()) 0)
(check-expect (count 2 (list 1 2 3 2 4 2 5 2 6 2)) 5)
(check-expect (count "hello" (list "world" "hello" "planet" "hello" "earth")) 2)

; from figure 72 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; [List-of Number] -> [List-of Number]
; produces a sorted version of l
(define (sort< l)
  (cond
    [(empty? l) '()]
    [(cons? l) (insert (first l) (sort< (rest l)))]))

(check-expect (sort< '()) '())
(check-expect (sort< (list 23)) (list 23))
(check-expect (sort< (list 11 8 14 7)) (list 7 8 11 14))
(check-expect (sort< (list 11 9 2 18 12 14 4 1))
              (list 1 2 4 9 11 12 14 18))
 
; Number [List-of Number] -> [List-of Number]
; inserts n into the sorted list of numbers l 
(define (insert n l)
  (cond
    [(empty? l) (cons n '())]
    [else (if (<= n (first l))
              (cons n l)
              (cons (first l) (insert n (rest l))))]))

(check-expect (insert 7 (range -10 11 1))
              (append (range -10 7 1) (list 7 7 8 9 10)))

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; NOTE ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Until about lists of length 50 there is no discernible difference in the time
; taken by the two functions. Below that, "time" returns 0 ms for both.
; Therefore 50 has been chosen to be the cross-over point. Above a length of
; 50 quick-sort< is significantly faster than sort<.

; END NOTE ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define numbers (create-tests 1000))

;(time (sort< numbers))
;(time (quick-sort< numbers))

; [List-of Number] -> [List-of Number]
; Produce a sorted version of l.
(define (clever-sort l)
  (cond
    [(< (length l) THRESHOLD)
     (sort< l)]
    [else
     (quick-sort< l)]))

(check-expect (clever-sort '()) '())
(check-expect (clever-sort (list 23)) (list 23))
(check-expect (clever-sort (list 11 8 14 7)) (list 7 8 11 14))
(check-expect (clever-sort (list 11 9 2 18 12 14 4 1))
              (list 1 2 4 9 11 12 14 18))

(time (quick-sort< numbers))
(time (clever-sort numbers))