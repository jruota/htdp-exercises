;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex197-1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A Letter is one of the following 1Strings: 
; – "a"
; – ... 
; – "z"
; or, equivalently, a member? of this list: 
(define LETTERS
  (explode "abcdefghijklmnopqrstuvwxyz"))

; A List-of-Letters is one of:
; – '()
; – (cons Letter List-of-Letters)
; Interpretation:
;     The collection of all lists containing
;     Letters only.
; NOTE
;     The definition of Letter above is widened
;     to encompass all units of any writing system, i.e.
;     including letters like ö, ł, í, etc.

(define-struct lc [letter count])
; A LetterCount is a structure:
;     (make-lc 1String Number)
; Interpretation:
;     A Letter and its number of occurence as
;     the starting letter.

; A List-of-LetterCounts is one of:
; – '()
; – (cons LetterCount List-of-LetterCounts)
; Interpretation:
;     A list containing LetterCounts.

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define LOCATION "/usr/share/dict/words")

; A Dictionary is a List-of-strings.
(define AS-LIST (read-lines LOCATION))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Dictionary -> LetterCount
; Produce the Letter-Count for the letter that occurs
; most often as the first one in the given Dictionary.
(define (most-frequent d)
  (cond
    [(empty? d) (make-lc "" 0)]
    [(cons? d)
     (first (sort-letter-counts> (count-by-letter d)))]))

(check-expect (most-frequent empty)
              (make-lc "" 0))
(check-expect (most-frequent (list "Alabama"
                                   "Alaska"
                                   "Arkansas"
                                   "Louisiana"
                                   "Massachussetts"
                                   "Wyoming"))
              (make-lc "A" 3))

; List-of-LetterCounts -> List-of-LetterCounts
; Sort the letter counts by number of occurences as
; a first letter in descending order.
(define (sort-letter-counts> lolc)
  (cond
    [(empty? lolc) '()]
    [(cons? lolc)
     (insert-lc (first lolc)
                (sort-letter-counts> (rest lolc)))]))

(check-expect (sort-letter-counts> '())
              '())
(check-expect (sort-letter-counts> (list (make-lc "a" 8235)
                                         (make-lc "b" 6338)
                                         (make-lc "c" 2409)
                                         (make-lc "d" 149)
                                         (make-lc "e" 2184)
                                         (make-lc "f" 474)))
              (list (make-lc "a" 8235)
                    (make-lc "b" 6338)
                    (make-lc "c" 2409)
                    (make-lc "e" 2184)
                    (make-lc "f" 474)
                    (make-lc "d" 149)))

; LetterCount List-of-LetterCounts -> List-of-LetterCounts
; Insert lc at the right place according to its count
; in the sorted lolc.
(define (insert-lc lc lolc)
  (cond
    [(empty? lolc) (list lc)]
    [(cons? lolc)
     (if (lc>=? lc (first lolc))
         (cons lc lolc)
         (cons (first lolc)
               (insert-lc lc (rest lolc))))]))

(check-expect (insert-lc (make-lc "a" 10) '())
              (list (make-lc "a" 10)))
(check-expect (insert-lc (make-lc "e" 2184)
                         (list (make-lc "a" 8235)
                               (make-lc "b" 6338)
                               (make-lc "c" 2409)
                               (make-lc "f" 474)
                               (make-lc "d" 149)))
              (list (make-lc "a" 8235)
                    (make-lc "b" 6338)
                    (make-lc "c" 2409)
                    (make-lc "e" 2184)
                    (make-lc "f" 474)
                    (make-lc "d" 149)))

; LetterCount LetterCount -> Boolean
; Does lc1 have a greater or equal count
; than lc2?
(define (lc>=? lc1 lc2)
  (>= (lc-count lc1) (lc-count lc2)))

(check-expect (lc>=? (make-lc "a" 10)
                     (make-lc "a" 100))
              #false)
(check-expect (lc>=? (make-lc "a" 2)
                     (make-lc "a" 2))
              #true)
(check-expect (lc>=? (make-lc "a" 123)
                     (make-lc "b" 23))
              #true)

; from exercise 196 (ex196.rkt) - - - - - - - - - - - - - - - - - - - - - - - -

; Dictionary -> List-of-LetterCounts
; Count how often each letter is used as the
; first one of a word in the given dictionary d.
(define (count-by-letter d)
  (cond
    [(empty? d) '()]
    [(cons? d)
     (count-starting-letters (starting-letters d) d)]))

; List-of-Letters Dictionary -> List-of-LetterCounts
; Count how often each letter in lol is used as the
; first one of a word in the given dictionary d.
(define (count-starting-letters lol d)
  (cond
    [(empty? lol) '()]
    [(cons? lol)
     (cons (make-lc (first lol)
                    (starts-with# (first lol) d))
           (count-starting-letters (rest lol) d))]))

; Dictionary -> List-of-Letters
; Extract all starting letters from the
; entries in d without duplicates.
(define (starting-letters d)
  (cond
    [(empty? d) '()]
    [(cons? d)
     (remove-duplicates
      (cons (substring (first d) 0 1)
            (starting-letters (rest d))))]))

; List-of-Letters -> List-of-Letters
; Remove all duplicates from lol.
(define (remove-duplicates lol)
  (cond
    [(empty? lol) '()]
    [(cons? lol)
     (if (member? (first lol) (rest lol))
         (remove-duplicates (rest lol))
         (cons (first lol)
               (remove-duplicates (rest lol))))]))

; from exercise 195 (ex195.rkt) - - - - - - - - - - - - - - - - - - - - - - - - 

; Letter Dictionary -> Number
; Count how many words in d start with l.
(define (starts-with# l d)
  (cond
    [(empty? d) 0]
    [(cons? d)
     (if (starts-with? l (first d))
         (+ 1 (starts-with# l (rest d)))
         (starts-with# l (rest d)))]))

; Letter String -> Boolean
; Does s start with l?
(define (starts-with? l s)
  (cond
    [(< 0 (string-length s))
     (string=? l (substring s 0 1))]
    [else #false]))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(most-frequent AS-LIST)

