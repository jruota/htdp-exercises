;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex196) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

; Dictionary -> List-of-LetterCounts
; Count how often each letter is used as the
; first one of a word in the given dictionary d.
(define (count-by-letter d)
  (cond
    [(empty? d) '()]
    [(cons? d)
     (count-starting-letters (starting-letters d) d)]))

(check-expect (count-by-letter '())
              '())
(check-expect (count-by-letter (list "Alabama"
                                     "Arkansas"
                                     "alaska"
                                     "Belgium"
                                     "burkina Faso"
                                     "Germany"
                                     "greece"
                                     "Zimbabwe"))
              (list (make-lc "A" 2)
                    (make-lc "a" 1)
                    (make-lc "B" 1)
                    (make-lc "b" 1)
                    (make-lc "G" 1)
                    (make-lc "g" 1)
                    (make-lc "Z" 1)))

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

(check-expect (count-starting-letters '() '())
              '())
(check-expect (count-starting-letters '()
                                      (list "Alabama"
                                            "Arkansas"
                                            "alaska"
                                            "Belgium"
                                            "burkina Faso"
                                            "Germany"
                                            "greece"
                                            "Zimbabwe"))
              '())
(check-expect (count-starting-letters (list "A"
                                            "B"
                                            "z")
                                      '())
              (list (make-lc "A" 0)
                    (make-lc "B" 0)
                    (make-lc "z" 0)))
(check-expect (count-starting-letters (list "A"
                                            "B"
                                            "z")
                                      (list "Alabama"
                                            "Arkansas"
                                            "alaska"
                                            "Belgium"
                                            "burkina Faso"
                                            "Germany"
                                            "greece"
                                            "Zimbabwe"))
              (list (make-lc "A" 2)
                    (make-lc "B" 1)
                    (make-lc "z" 0)))

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

(check-expect (starting-letters '())
              '())
(check-expect (starting-letters (list "Alabama"
                                      "Arkansas"
                                      "alaska"
                                      "Belgium"
                                      "burkina Faso"
                                      "Germany"
                                      "greece"
                                      "Zimbabwe"))
              (list "A" "a" "B" "b" "G" "g" "Z"))

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

(check-expect (remove-duplicates '())
              '())
(check-expect (remove-duplicates (list "a" "a" "a" "b" "b" "c" "d" "e" "f" "f"))
              (list "a" "b" "c" "d" "e" "f"))

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

(count-by-letter AS-LIST)
