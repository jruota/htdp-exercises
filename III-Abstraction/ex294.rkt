;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex294) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; CONSTANTS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define ERR-MSG1
  "get-at-index: expects an integer i where 0 <= i <= (length l)")
(define ERR-MSG2
  "sublist: integers n and m have to be greater than or equal to 0")

; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; N is one of:
; – 0
; – (add1 N)
; Interpretation:
;     The natural numbers, including 0.

; A [Maybe X] is one of: 
; – #false 
; – X

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; X [List-of X] -> [Maybe N]
; determine the index of the first occurrence
; of x in l, #false otherwise
(define (index x l)
  (cond
    [(empty? l) #false]
    [else
     (if (equal? (first l) x)
         0
         (local ((define i (index x (rest l))))
           (if (boolean? i)
               i
               (+ i 1))))]))

(check-satisfied (index 5 (list 1 2 3 4 5 6 7 8 9))
                 (is-index? 5 (list 1 2 3 4 5 6 7 8 9)))
(check-satisfied (index 5 (list 1 2 5 3 4 5 6 7 8 9))
                 (is-index? 5 (list 1 2 5 3 4 5 6 7 8 9)))

(check-satisfied (index 5 '())
                 (is-index? 5 '()))

; X [List-of X] -> [N -> Boolean]
; Is i the index of the first occurrence
; of x in l?
(define (is-index? x l)
  (lambda (i)
    (if (boolean? i)
        (not i)
        (and (equal? x (get-at-index i l))
             (not (member? x (sublist 0 i l)))))))

(check-expect ((is-index? 5 (list 1 2 5 3 4 5 6 7 8 9)) 2)
              #true)
(check-expect ((is-index? 5 (list 1 2 5 3 4 5 6 7 8 9)) 5)
              #false)

; N [List-of X] -> [Maybe X]
; Return the value at index i in l,
; where i is a natural number and less than
; the length of l.
(define (get-at-index i l)
  (if (and (integer? i)
           (>= i 0)
           (< i (length l)))
      (cond
        [(empty? l) #false]    ; redundant, because of testing of i
        [(cons? l)
         (if (= i 0)
             (first l)
             (get-at-index (sub1 i) (rest l)))])
      (error ERR-MSG1)))

(check-error (get-at-index 1.23 (list 1 2 3 4 5 6 7 8 9))
             ERR-MSG1)
(check-error (get-at-index -1 (list 1 2 3 4 5 6 7 8 9))
             ERR-MSG1)
(check-error (get-at-index 23 (list 1 2 3 4 5 6 7 8 9))
             ERR-MSG1)

(check-expect (get-at-index 0 (list 1 2 3 4 5 6 7 8 9))
              1)
(check-expect (get-at-index 4 (list 1 2 3 4 5 6 7 8 9))
              5)
(check-expect (get-at-index 8 (list 1 2 3 4 5 6 7 8 9))
              9)

; N N [List-of X] -> [List-of X]
; Return the values of l between n (inclusive)
; and m (exclusive). If m is greater than
; the length of the list, return all values
; from n to the end of the list.
(define (sublist n m l)
  (if (and (integer? n)
           (integer? m)
           (>= n 0)
           (>= m 0))
      (if (or (empty? l)
              (>= n m))
          '()
          (if (zero? n)
              (cons (first l)
                    (sublist n (sub1 m) (rest l)))
              (sublist (sub1 n) (sub1 m) (rest l))))
      (error ERR-MSG2)))

(check-error (sublist 3.14 5 (list 1 2 3 4 5 6 7 8 9))
             ERR-MSG2)
(check-error (sublist 3 5.14 (list 1 2 3 4 5 6 7 8 9))
             ERR-MSG2)
(check-error (sublist -1 5 (list 1 2 3 4 5 6 7 8 9))
             ERR-MSG2)
(check-error (sublist 3 -1 (list 1 2 3 4 5 6 7 8 9))
             ERR-MSG2)

(check-expect (sublist 3 5 '())
              '())
(check-expect (sublist 5 3 (list 1 2 3 4 5 6 7 8 9))
              '())

(check-expect (sublist 3 5 (list 1 2 3 4 5 6 7 8 9))
              (list 4 5))