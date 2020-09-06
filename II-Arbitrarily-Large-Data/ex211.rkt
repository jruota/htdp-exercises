;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex211) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

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

; A List-of-strings is one of:
; – '()
; – (cons String List-of-strings)
; Interpretation:
;     A list containing strings.

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 
(define LOCATION "/usr/share/dict/words")
 
; A Dictionary is a List-of-strings.
(define AS-LIST (read-lines LOCATION))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 
; List-of-strings -> List-of-strings
; picks out all those Strings that occur in the dictionary 
(define (in-dictionary los)
  (cond
    [(empty? los) '()]
    [(cons? los)
     (if (member? (first los) AS-LIST)
         (cons (first los)
               (in-dictionary (rest los)))
         (in-dictionary (rest los)))]))

(check-expect (in-dictionary '())
              '())
(check-expect (in-dictionary (list "apple"
                                   "arghbtz"
                                   "banana"
                                   "orange"))
              (list "apple"
                    "banana"
                    "orange"))

; from exercise 210 (ex210.rkt) ------------------------------------------------

; List-of-words -> List-of-strings
; turns all Words in low into Strings 
(define (words->strings low)
  (cond
    [(empty? low) '()]
    [(cons? low)
     (cons (word->string (first low))
           (words->strings (rest low)))]))

(check-expect (words->strings '())
              '())
(check-expect (words->strings (list (list "a" "p" "p" "l" "e")
                                    '()
                                    (list "b" "a" "n" "a" "n" "a")))
              (list "apple" "" "banana"))

; from exercise 209 (ex209.rkt) ------------------------------------------------

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