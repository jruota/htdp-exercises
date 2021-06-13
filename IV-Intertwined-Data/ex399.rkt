;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex399) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define SISTERS (list "Louise" "Jane" "Laura" "Dana" "Mary"))

(define INDEX-ERROR "index is out of range")
(define LENGTH-ERROR "lists must have the same length")

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; [NEList-of X] -> X 
; returns a random item from the list 
(define (random-pick l)
  (index l (random (length l))))

(check-member-of (random-pick SISTERS)
                 "Louise" "Jane" "Laura" "Dana" "Mary")
 
; [List-of String] [List-of [List-of String]] 
; -> 
; [List-of [List-of String]]
; produces the list of those lists in ll that do 
; not agree with names at any place 
(define (non-same names ll)
  (cond
    [(empty? ll) '()]
    [(empty? names) ll]
    [(cons? names)
     (if (different-at-every-place? names (first ll))
         (cons (first ll)
               (non-same names (rest ll)))
         (non-same names (rest ll)))]))

(check-expect (non-same '() '())
              '())
(check-expect (non-same '()
                        (list (list "a" "b" "c")
                              (list "b" "a" "c")
                              (list "b" "c" "a")
                              (list "a" "c" "b")
                              (list "c" "a" "b")
                              (list "c" "b" "a")))
              (list (list "a" "b" "c")
                    (list "b" "a" "c")
                    (list "b" "c" "a")
                    (list "a" "c" "b")
                    (list "c" "a" "b")
                    (list "c" "b" "a")))
(check-expect (non-same (list "a" "b" "c") '())
              '())
(check-expect (non-same (list "a" "b" "c")
                        (list (list "a" "b" "c")
                              (list "b" "a" "c")
                              (list "b" "c" "a")
                              (list "a" "c" "b")
                              (list "c" "a" "b")
                              (list "c" "b" "a")))
              (list (list "b" "c" "a")
                    (list "c" "a" "b")))

; [List-of X] N -> X
; Get the element at index n in lox,
; signal an error if the list is too short.
(define (index lox n)
  (cond
    [(empty? lox) (error INDEX-ERROR)]
    [(zero? n) (first lox)]
    [(> n 0) (index (rest lox) (sub1 n))]))

(check-error (index '() 0)
             INDEX-ERROR)
(check-error (index '() 5)
             INDEX-ERROR)
(check-error (index SISTERS 5)
             INDEX-ERROR)
(check-expect (index SISTERS 0)
              "Louise")
(check-expect (index SISTERS 4)
              "Mary")

; [List-of X] [List-of X] -> Boolean
; Do the two same-length lists l1 and l2
; have different elements at the corresponding
; indices?
(define (different-at-every-place? l1 l2)
  (cond
    [(and (empty? l1) (empty? l2)) #true]
    [(and (empty? l1) (cons? l2)) (error LENGTH-ERROR)]
    [(and (cons? l1) (empty? l2)) (error LENGTH-ERROR)]
    [(and (cons? l1) (cons? l2))
     (and (not (equal? (first l1) (first l2)))
          (different-at-every-place? (rest l1) (rest l2)))]))

(check-expect (different-at-every-place? '() '())
              #true)
(check-error (different-at-every-place? (list "a" "b" "c")
                                        '())
             LENGTH-ERROR)
(check-error (different-at-every-place? '()
                                        (list "a" "b" "c"))
             LENGTH-ERROR)
(check-expect (different-at-every-place? (list "a" "b" "c")
                                         (list "a" "b" "c"))
              #false)
(check-expect (different-at-every-place? (list "a" "b" "c")
                                         (list "b" "a" "c"))
              #false)
(check-expect (different-at-every-place? (list "a" "b" "c")
                                         (list "b" "c" "a"))
              #true)
(check-error (different-at-every-place? (list "a" "b" "c")
                                        (list "d" "e" "f" "g"))
             LENGTH-ERROR)
(check-error (different-at-every-place? (list "a" "b" "c")
                                        (list "d" "e"))
             LENGTH-ERROR)