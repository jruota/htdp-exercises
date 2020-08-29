;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex212) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A Word is one of:
; – '() or
; – (cons 1String Word)
; interpretation a Word is a list of 1Strings (letters)

; A List-of-words is one of:
; – '()
; – (cons Word List-of-words)
; Interpretation:
;     A list containing words.

; DATA EXAMPLES ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define WORD1 empty)
(define WORD2 (list "a" "p" "p" "l" "e"))
(define WORD3 (list "b" "a" "n" "a" "n" "a"))
(define WORD4 (list "d" "e"))

(define LOW1 (list WORD1 WORD2 WORD3))
(define LOW2 (list (list "d" "e")
                   (list "e" "d")))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Word -> List-of-words
; creates all rearrangements of the letters in w
(define (arrangements w)
  '())

(check-expect (arrangements '())
              '())    ; -> different than in the book
(check-member-of (arrangements WORD4)
                 LOW2
                 (reverse LOW2))
              