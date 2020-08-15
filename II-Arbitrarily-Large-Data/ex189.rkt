;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex189) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A List-of-numbers is one of: 
; – '()
; – (cons Number List-of-numbers)

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Number List-of-numbers -> Boolean
; Check whether n is in alon.
(define (search n alon)
  (cond
    [(empty? alon) #false]
    [else (or (= (first alon) n)
              (search n (rest alon)))]))

(check-expect (search 1 '())
              #false)
(check-expect (search 1 (list 9 8 7 6 5 4 3 2 1 0))
              #true)
(check-expect (search 1 (list 9 8 7 6 5 4 3 2 0))
              #false)
(check-expect (search 1 (list 9 3 6 7 8 1 0 5 4 2))
              #true)
(check-expect (search 1 (list 9 3 6 7 8 0 5 4 2))
              #false)

; Number List-of-numbers -> Boolean
; Check whether n is in the sorted list alon (descending).
(define (search-sorted n alon)
  (cond
    [(empty? alon) #false]
    [(cons? alon)
     (if (> n (first alon))
         #false
         (or (= n (first alon))
             (search-sorted n (rest alon))))]))

(check-expect (search-sorted 6 '())
              #false)
(check-expect (search-sorted 6 (list 9 8 7 6 5 4 3 2 1 0))
              #true)
(check-expect (search-sorted 6 (list 9 8 7 5 4 3 2 1 0))
              #false)