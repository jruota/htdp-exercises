;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex253) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [Number -> Boolean]
; even?, odd?, positive?, negative?
(even? 2)
(odd? 2)
(positive? -3.1234)
(negative? -3.1234)

; [Boolean String -> Boolean]
(define (do-nothing b s)
  b)

(check-expect (do-nothing #true "Sweden")
              #true)
(check-expect (do-nothing #false "Sweden")
              #false)

; [Number Number Number -> Number]
(define (trisum a b c)
  (+ a b c))

(check-expect (trisum 1 2 3)
              6)

; [Number -> [List-of Number]]
(define (make-nlist n)
  (cond
    [(negative? n) '()]
    [(or (positive? n)
         (zero? n))
     (cons n
           (make-nlist (sub1 n)))]))

(check-expect (make-nlist -1)
              '())
(check-expect (make-nlist 0)
              (list 0))
(check-expect (make-nlist 10)
              (list 10 9 8 7 6 5 4 3 2 1 0))

; [[List-of Number] -> Boolean]
(define (any-even? lon)
  (cond
    [(empty? lon) #false]
    [(cons? lon)
     (or (even? (first lon))
         (any-even? (rest lon)))]))

(check-expect (any-even? (list 1 3 5 7 9))
              #false)
(check-expect (any-even? (list 1 3 5 7 8 9))
              #true)