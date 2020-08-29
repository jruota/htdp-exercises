;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex209) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A Word is one of:
; – '() or
; – (cons 1String Word)
; interpretation a Word is a list of 1Strings (letters)

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; String -> Word
; converts s to the chosen word representation 
(define (string->word s)
  (explode s))

(check-expect (string->word "")
              '())
(check-expect (string->word "apple")
              (list "a" "p" "p" "l" "e"))

; Word -> String
; converts w to a string
(define (word->string w)
  (implode w))

(check-expect (word->string '())
              "")
(check-expect (word->string (list "a" "p" "p" "l" "e"))
              "apple")
