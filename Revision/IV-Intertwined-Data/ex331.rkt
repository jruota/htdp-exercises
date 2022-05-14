;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex331) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A Dir.v1 (short for directory) is one of: 
; – '()
; – (cons File.v1 Dir.v1)
; – (cons Dir.v1 Dir.v1)
 
; A File.v1 is a String.

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define CODE (list "hang" "draw"))
(define DOCS (list "read!"))
(define LIBS (list CODE DOCS))
(define TEXT (list "part1" "part2" "part3"))
(define TS (list TEXT "read!" LIBS))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Dir.v1 -> N
; Return how many files the directory dir contains.
(define (how-many dir)
  (cond
    [(empty? dir) 0]
    [else
     (local ((define item (first dir)))
       ; – IN –
       (cond
         [(string? item)
          (+ 1 (how-many (rest dir)))]
         [(cons? item)
          (+ (how-many item)
             (how-many (rest dir)))]))]))

(check-expect (how-many '()) 0)
(check-expect (how-many TS) 7)
(check-expect (how-many TEXT) 3)
(check-expect (how-many LIBS) 3)
(check-expect (how-many CODE) 2)
(check-expect (how-many DOCS) 1)
