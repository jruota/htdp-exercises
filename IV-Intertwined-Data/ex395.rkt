;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex395) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; An N is one of:
; – 0
; – (add1 N)
; Interpretation:
;     The natural numbers.

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; [List-of X] N -> [List-of X]
; Produce the first n elements of l or
; the complete list l if it has a length
; shorter than n.
(define (take l n)
;  (cond
;    [(empty? l) '()]
;    [else
;     (if (zero? n)
;         '()
;         (cons (first l) (take (rest l) (sub1 n))))]))
  (cond
    [(empty? l) l]
    [(zero? n) '()]
    [(> n 0)
     (cons (first l)
           (take (rest l) (sub1 n)))]))

(check-expect (take '() 3)
              '())
(check-expect (take '(a b c d e f g h i j k) 0)
              '())
(check-expect (take '(a b c d e f g h i j k) 5)
              '(a b c d e))
(check-expect (take '(a b c d e f g h i j k) 15)
              '(a b c d e f g h i j k))

; [List-of X] N -> [List-of X]
; Return l with the first l elements removed
; or '() if l is too short.
(define (drop l n)
  (cond
    [(empty? l) '()]
    [(zero? n) l]
    [(> n 0)
     (drop (rest l) (sub1 n))]))

(check-expect (drop '() 3)
              '())
(check-expect (drop '() 0)
              '())
(check-expect (drop '(a b c d e f g h i j k) 0)
              '(a b c d e f g h i j k))
(check-expect (drop '(a b c d e f g h i j k) 5)
              '(f g h i j k))
(check-expect (drop '(a b c d e f g h i j k) 15)
              '())