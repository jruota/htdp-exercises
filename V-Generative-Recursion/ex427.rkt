;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex427) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define THRESHOLD 3)

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; [List-of Number] -> [List-of Number]
; produces a sorted version of alon
(define (quick-sort< alon)
  (cond
    [(empty? alon) '()]
    [(> THRESHOLD (length alon)) (sort< alon)]
    [else
     (append (quick-sort< (smallers alon (first alon)))
             (list (first alon))
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

; from figure 72 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; [List-of Number] -> [List-of Number]
; produces a sorted version of l
(define (sort< l)
  (cond
    [(empty? l) '()]
    [(cons? l) (insert (first l) (sort< (rest l)))]))
 
; Number [List-of Number] -> [List-of Number]
; inserts n into the sorted list of numbers l 
(define (insert n l)
  (cond
    [(empty? l) (cons n '())]
    [else (if (<= n (first l))
              (cons n l)
              (cons (first l) (insert n (rest l))))]))