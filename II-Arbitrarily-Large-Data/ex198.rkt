;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex198) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

; A NEList-of-LetterCounts (non-empty) is one of:
; – (cons LetterCount '())
; – (cons LetterCount NEList-of-LetterCounts)
; Interpretation:
;     A non-empty list of LetterCounts.

(define LOCATION "/usr/share/dict/words")
; A Dictionary is a List-of-strings.
(define AS-LIST (read-lines LOCATION))

; A List-of-Dictionaries is one of:
; – '()
; – (cons Dictionary List-of-Dictionaries)
; Interpretation:
;     A list of Dictionaries, one per Letter.

; A NEList-of-Dictionaries (non-empty) is one of:
; – (cons Dictionary '())
; – (cons Dictionary NEList-of-Dictionaries)
; Interpretation:
;     A non-empty list of Dictionaries.

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Dictionary -> List-of-Dictionaries
; Produce a list of Dictionaries, one per Letter.
(define (words-by-first-letter d)
  (cond
    [(empty? d) '()]
    [(cons? d)
     (dictionaries-by-letter (starting-letters d) d)]))
     

(check-expect (words-by-first-letter '())
              '())
(check-expect (words-by-first-letter (list "Alabama"
                                           "Alaska"
                                           "Arkansas"
                                           "Louisiana"
                                           "Massachussetts"
                                           "Wyoming"))
              (list (list "Alabama"
                          "Alaska"
                          "Arkansas")
                    (list "Louisiana")
                    (list "Massachussetts")
                    (list "Wyoming")))

; Dictionary -> LetterCount
; Produce the Letter-Count for the letter that occurs
; most often as the first one in the given Dictionary.
(define (most-frequent.v2 d)
  (cond
    [(empty? d) (make-lc "" 0)]
    [(cons? d)
     (make-lc (substring (first (longest-dict (words-by-first-letter d)))
                         0 1)
              (length (longest-dict (words-by-first-letter d))))]))

(check-expect (most-frequent.v2 '())
              (make-lc "" 0))
(check-expect (most-frequent AS-LIST)
              (most-frequent.v2 AS-LIST))

; List-of-Letters Dictionary -> List-of-Dictionaries
; Return a list of Dictionaries, one per letter in lol.
(define (dictionaries-by-letter lol d)
  (cond
    [(empty? lol) '()]
    [(cons? lol)
     (cons (words-by-letter (first lol) d)
           (dictionaries-by-letter (rest lol) d))]))

(check-expect (dictionaries-by-letter '() '())
              '())
(check-expect (dictionaries-by-letter (list "A" "B" "L" "W" "m")
                                      '())
              (list empty empty empty empty empty))
(check-expect (dictionaries-by-letter '()
                                      (list "Alabama"
                                            "Alaska"
                                            "Arkansas"
                                            "Louisiana"
                                            "Massachussetts"
                                            "Wyoming"))
              '())
(check-expect (dictionaries-by-letter (list "A" "B" "L" "W" "m")
                                      (list "Alabama"
                                            "Alaska"
                                            "Arkansas"
                                            "Louisiana"
                                            "Massachussetts"
                                            "Wyoming"))
              (list (list "Alabama"
                          "Alaska"
                          "Arkansas")
                    '()
                    (list "Louisiana")
                    (list "Wyoming")
                    '()))

; Letter Dictionary -> Dictionary
; Return a Dictionary containing all words
; starting with l in d.
(define (words-by-letter l d)
  (cond
    [(empty? d) '()]
    [(cons? d)
     (if (string=? l (substring (first d) 0 1))
         (cons (first d) (words-by-letter l (rest d)))
         (words-by-letter l (rest d)))]))

(check-expect (words-by-letter "A" '())
              '())
(check-expect (words-by-letter "A" (list "Alabama"
                                         "Alaska"
                                         "Arkansas"
                                         "Louisiana"
                                         "Massachussetts"
                                         "Wyoming"))
              (list "Alabama"
                    "Alaska"
                    "Arkansas"))

; NEList-of-Dictionaries -> Dictionary
; Return the longest Dictionary in lod.
(define (longest-dict lod)
  (cond
    [(empty? (rest lod)) (first lod)]
    [(cons? lod)
     (if (>= (length (first lod))
             (length (second lod)))
         (longest-dict (remove (second lod) lod))
         (longest-dict (remove (first lod) lod)))]))

(check-expect (longest-dict (list (list "Berlin")))
              (list "Berlin"))
(check-expect (longest-dict  (list (list "Alabama"
                                         "Alaska"
                                         "Arkansas")
                                   '()
                                   (list "Louisiana")
                                   (list "Wyoming")
                                   '()))
              (list "Alabama"
                    "Alaska"
                    "Arkansas"))

; from exercise 197 (ex197.rkt) ------------------------------------------------

; Dictionary -> LetterCount
; Produce the Letter-Count for the letter that occurs
; most often as the first one in the given Dictionary.
(define (most-frequent d)
  (cond
    [(empty? d) (make-lc "" 0)]
    [(cons? d)
     (max-lc (count-by-letter d))]))

(check-expect (most-frequent empty)
              (make-lc "" 0))
(check-expect (most-frequent (list "Alabama"
                                   "Alaska"
                                   "Arkansas"
                                   "Louisiana"
                                   "Massachussetts"
                                   "Wyoming"))
              (make-lc "A" 3))

; NEList-of-LetterCounts -> LetterCount
; Return the LetterCount with the highest count in nelolc.
(define (max-lc nelolc)
  (cond
    [(empty? (rest nelolc)) (first nelolc)]
    [else
     (if (lc>=? (first nelolc) (second nelolc))
         (max-lc (remove (second nelolc) nelolc))
         (max-lc (remove (first nelolc) nelolc)))]))

(check-expect (max-lc (list (make-lc "b" 123)))
              (make-lc "b" 123))
(check-expect (max-lc (list (make-lc "b" 6338)
                            (make-lc "c" 2409)
                            (make-lc "d" 149)
                            (make-lc "e" 2184)
                            (make-lc "f" 474)
                            (make-lc "a" 8235)))
              (make-lc "a" 8235))

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

; from exercise 196 (ex196.rkt) ------------------------------------------------

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

; ------------------------------------------------------------------------------

; (words-by-first-letter AS-LIST)