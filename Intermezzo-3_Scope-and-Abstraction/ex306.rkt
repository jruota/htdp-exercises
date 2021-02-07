;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex306) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)

; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A NNumber+ (positive natural number) is one of
;     – 1
;     – (add1 NNumber+)
; Interpretation:
;     The natural numbers without zero.

; A Row is one of: 
;  – '() 
;  – (cons Number Row)

; A Matrix is one of: 
;  – (cons Row '())
;  – (cons Row Matrix)
; Constraint:
;     all rows in matrix are of the same length.

; An IM (identity matrix) is a square Matrix
; with ones on the main diagonal and
; zeros elsewhere.
; Constraint:
;     An IM has at least the dimension 1.

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; N -> [List-of N]
; Create the list (list 0 ... (- n 1))
; for any natural number n.
(define (naturals-list n)
  (for/list ([m n])
    m))

(check-expect (naturals-list 0)
              '())
(check-expect (naturals-list 1)
              (list 0))
(check-expect (naturals-list 10)
              (list 0 1 2 3 4 5 6 7 8 9))

; N -> [List-of N]
; Create the list (list 1 ... n)
; for any natural number n.
(define (naturals-list-w/o-zero n)
  (for/list ([m n])
    (add1 m)))

(check-expect (naturals-list-w/o-zero 0)
              '())
(check-expect (naturals-list-w/o-zero 1)
              (list 1))
(check-expect (naturals-list-w/o-zero 10)
              (list 1 2 3 4 5 6 7 8 9 10))

; N -> [List-of Number]
; Create the list (list 1 1/2 ... 1/n)
; for any natural number n.
(define (list-of-fractions n)
  (for/list ([m n])
    (/ 1 (add1 m))))

(check-expect (list-of-fractions 0)
              '())
(check-expect (list-of-fractions 1)
              (list 1))
(check-expect (list-of-fractions 5)
              (list 1 1/2 1/3 1/4 1/5))

; N -> [List-of N]
; Create the list of the first n even numbers.
(define (first-n-even n)
  (for/list ([m (in-range 0 (* 2 n) 2)])
    m))

(check-expect (first-n-even 0)
              '())
(check-expect (first-n-even 1)
              (list 0))
(check-expect (first-n-even 10)
              (list 0 2 4 6 8 10 12 14 16 18))
(check-expect (first-n-even 11)
              (list 0 2 4 6 8 10 12 14 16 18 20))

; NNumber+ -> IM
; Return an identity matrix of size n.
(define (identityM n)
  (for*/list ([m n])
    (build-list n (lambda (x) (if (= x m) 1 0)))))

(check-expect (identityM 1)
              (list (list 1)))

(check-expect (identityM 2)
              (list (list 1 0)
                    (list 0 1)))

(check-expect (identityM 5)
              (list (list 1 0 0 0 0)
                    (list 0 1 0 0 0)
                    (list 0 0 1 0 0)
                    (list 0 0 0 1 0)
                    (list 0 0 0 0 1)))

; Number [Number -> Number] -> [List-of Number]
; Tabulate f between n
; and 0 (inclusive) in a list.
(define (tabulate n f)
  (reverse
   (for/list ([x (add1 n)])
     (f x))))

(check-within (tabulate 0 sin)
              (list (sin 0))
              .0001)
(check-within (tabulate 4 sqrt)
              (list (sqrt 4) (sqrt 3) (sqrt 2) (sqrt 1) (sqrt 0))
              .0001)