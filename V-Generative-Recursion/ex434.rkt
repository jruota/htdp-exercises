;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex434) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; NOTE ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; This version of smallers will keep the first element of the input list
; (because of its non-strict comparison), which usually is the pivot in
; the quick-sort function. Thus, instead of being removed from the original
; list, the pivot is added for each recursive call to quick-sort.

; This can lead to an infinite loop. Take (list 11 8 14 7) as an example.
; The first call to smallers will return (list 11 8 14), with 11 as the pivot.
; The first recursive call is then (quick-sort< (list 11 8 14)), 11 is the pivot
; again and smallers return (list 11 8 14) again, i.e. the list does not get
; smaller. Instead to get the (intermediate) result of (quick-sort< 11 8 14)
; the algorithm needs to solve (quick-sort< 11 8 14).

; END NOTE ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; [List-of Number] -> [List-of Number]
; produces a sorted version of alon
; assume the numbers are all distinct 
(define (quick-sort< alon)
  (cond
    [(empty? alon) '()]
    [else (local ((define pivot (first alon)))
            (append (quick-sort< (smallers alon pivot))
                    (list pivot)
                    (quick-sort< (largers alon pivot))))]))

(check-expect (quick-sort< '()) '())
;(check-expect (quick-sort< (list 11 8 14 7)) (list 7 8 11 14))
;(check-expect (quick-sort< (list 11 9 2 18 12 14 4 1))
;              (list 1 2 4 9 11 12 14 18))

; [List-of Number] Number -> [List-of Number]
; Return all numbers from lon that are less
; than n.
(define (smallers l n)
  (cond
    [(empty? l) '()]
    [else (if (<= (first l) n)
              (cons (first l) (smallers (rest l) n))
              (smallers (rest l) n))]))

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