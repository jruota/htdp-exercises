;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex235) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; An Los (List-of-String) 
; is one of: 
;     – '() 
;     – (cons String Los)
; Interpretation:
;     A list of strings.

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define LIST-ONE '("ananas" "birmingham" "statistics" "dog" "tomato"))
(define LIST-TWO '("apple" "atom" "basic" "mathematics" "cow" "zoo"))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Los -> Boolean
; Does l contain the string "atom"?
(define (contains-atom? l)
  (contains? "atom" l))

(check-expect (contains-atom? LIST-ONE)
              #false)
(check-expect (contains-atom? LIST-TWO)
              #true)

; Los -> Boolean
; Does l contain the string "basic"?
(define (contains-basic? l)
  (contains? "basic" l))

(check-expect (contains-basic? LIST-ONE)
              #false)
(check-expect (contains-basic? LIST-TWO)
              #true)

; Los -> Boolean
; Does l contain the string "zoo"?
(define (contains-zoo? l)
  (contains? "zoo" l))

(check-expect (contains-zoo? LIST-ONE)
              #false)
(check-expect (contains-zoo? LIST-TWO)
              #true)

; String Los -> Boolean
; Does the list l contain the string s?
(define (contains? s l)
  (cond
    [(empty? l) #false]
    [else (or (string=? (first l) s)
              (contains? s (rest l)))]))

(check-expect (contains? "cow" '())
              #false)
(check-expect (contains? "cow" LIST-TWO)
              #true)