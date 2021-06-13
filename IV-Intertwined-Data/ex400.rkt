;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex400) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A DNA-Strand is one of the following:
; – '()
; – (cons Base DNA-Strand)
; Interpretation:
;     A sequence of nucleobases.

; A Base is one of:
; – "a"
; – "c"
; – "g"
; – "t"
; Interpretation
;     The four nucleobases found in DNA.

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define INDEX-ERROR "index is out of range")
(define PATTERN-ERROR "\"pattern\" has to be shorter than \"search\"")

; FOR WRITING TESTS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; first two functions are from ex399.rkt

; [NEList-of X] -> X 
; returns a random item from the list 
(define (random-pick l)
  (index l (random (length l))))

; [List-of X] N -> X
; Get the element at index n in lox,
; signal an error if the list is too short.
(define (index lox n)
  (cond
    [(empty? lox) (error INDEX-ERROR)]
    [(zero? n) (first lox)]
    [(> n 0) (index (rest lox) (sub1 n))]))

; N -> [List-of Base]
; A wrapper around the function random-pick,
; that discards its only argument.
; Intended for use with build-list.
(define (make-DNA-strand n)
  (random-pick (list "a" "c" "g" "t")))

; Functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; [List-of Base] [List-of Base] -> Boolean
; Return #true if pattern is identical to the initial
; part of search, #false otherwise.
(define (DNAprefix pattern search)
  (cond
    [(empty? pattern) #true]
    [(empty? search) #false]
    [(cons? search)
     (and (string=? (first pattern) (first search))
          (DNAprefix (rest pattern) (rest search)))]))

(check-expect (DNAprefix '() '())
              #true)
(check-expect (DNAprefix '() (list "c" "a" "a" "g" "g" "t" "c" "a" "a" "a"))
              #true)
(check-expect (DNAprefix (list "g" "c" "t" "a" "a") '())
              #false)
(check-expect (DNAprefix (list "g" "c" "t" "a" "a")
                         (list "c" "a" "g" "g" "c" "t" "g" "g" "g" "t"))
              #false)
(check-expect (DNAprefix (list "t" "g" "c" "a" "g")
                         (list "t" "g" "c" "a" "g" "a" "c" "t" "a" "g"))
              #true)
(check-expect (DNAprefix (list "c" "t" "t" "a" "a" "t" "t" "t" "a" "t")
                         (list "c" "t" "t" "a" "a" "t" "t" "t" "a" "t"))
              #true)
(check-expect (DNAprefix (list "c" "c" "g" "t" "g" "c" "t" "g" "t" "c" "c" "g")
                         (list "g" "g" "c" "t" "c" "a" "t" "g"))
              #false)

; [List-of Base] [List-of Base] -> Base
; Return the first element in search beyond
; pattern. If pattern does not match the beginning
; of search, return #false.
; If pattern and search are equal, signal an error.
(define (DNAdelta pattern search)
    (cond
      [(equal? pattern search) (error PATTERN-ERROR)]
      [else
       (if (and (cons? pattern) (DNAprefix pattern search))
           (index search (length pattern))
           #false)]))
;  (cond
;    [(and (empty? pattern) (empty? search)) (error PATTERN-ERROR)]
;    [(and (empty? pattern) (cons? search)) #false]
;    [(and (cons? pattern) (empty? search)) #false]
;    [(and (cons? pattern) (cons? search))
;     (if (DNAprefix pattern search)
;         (index search (length pattern))
;         #false)]))

(check-error (DNAdelta '() '())
             PATTERN-ERROR)
(check-expect (DNAdelta '() (list "t" "c" "a" "g" "g" "c" "t"))
              #false)
(check-expect (DNAdelta (list "a" "c" "g" "c" "a" "c") '())
              #false)
(check-expect (DNAdelta (list "a" "g" "g" "t")
                        (list "a" "g" "g" "t" "g" "t" "c" "a"))
              "g")