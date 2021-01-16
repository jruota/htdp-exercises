;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex275) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

; Dictionary -> List-of-Dictionaries
; Produce a list of Dictionaries, one per Letter.
(define (words-by-first-letter d)
  (dictionaries-by-letter (starting-letters d) d))   

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

; List-of-Letters Dictionary -> List-of-Dictionaries
; Return a list of Dictionaries, one per letter in lol.
(define (dictionaries-by-letter lol d)
  (local (; Letter -> Dictionary
          ; Return a Dictionary with all
          ; words from d that start with ltr.
          (define (words-by-letter-wrapper ltr)
            (words-by-letter ltr d)))
    ; – IN –
    (map words-by-letter-wrapper lol)))

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
  (filter (lambda (str) (string=? l (substring str 0 1))) d))

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

; ––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
; ––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––

; NOTE +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

; The selection of the max LetterCount may return different results if some
; LetterCounts have the same count, i.e. the results depends whether a strict
; comparison (<, >) or non-strict (<=, >=) comparison is used.

; END NOTE +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

; Dictionary -> LetterCount
; Produce the Letter-Count for the letter that occurs
; most often as the first one in the given Dictionary.
; Return #false if d is empty.
(define (most-frequent d)
  (local
    (; LetterCount LetterCount -> Boolean
     ; Does lc1 have a greater or equal count
     ; than lc2?
     (define (lc>? lc1 lc2)
       (> (lc-count lc1) (lc-count lc2)))
     
     (define result (sort (count-by-letter d) lc>?)))
    
    ; – IN –
    (if (empty? result)
        #false
        (first result))))

(check-expect (most-frequent empty)
              #false)
(check-expect (most-frequent (list "Alabama"
                                   "Alaska"
                                   "Arkansas"
                                   "Louisiana"
                                   "Massachussetts"
                                   "Wyoming"))
              (make-lc "A" 3))
(check-expect (most-frequent (list "Alabama"
                                   "Alaska"
                                   "Arkansas"
                                   "almond"
                                   "ananas"
                                   "apple"
                                   "Louisiana"
                                   "Massachussetts"
                                   "Wyoming"))
              (make-lc "A" 3))

; Dictionary -> LetterCount
; Produce the Letter-Count for the letter that occurs
; most often as the first one in the given Dictionary.
; Return #false if d is empty.
(define (most-frequent.v2 d)
  (local
    (; LetterCount LetterCount -> Boolean
     ; Does lc1 have a greater or equal count
     ; than lc2?
     (define (lc>? lc1 lc2)
       (> (lc-count lc1) (lc-count lc2)))

     ; NEList-of-LetterCounts -> LetterCount
     ; Return the LetterCount with the highest count in nelolc.
     (define (max-lc nelolc)
       (cond
         [(empty? (rest nelolc)) (first nelolc)]
         [else
          (if (lc>? (first nelolc) (second nelolc))
              ;(max-lc (remove (second nelolc) nelolc))
              (max-lc (cons (first nelolc) (rest (rest nelolc))))
              (max-lc (rest nelolc)))])))
    ; – IN –
    (if (empty? d)
        #false
        (max-lc (count-by-letter d)))))

(check-expect (most-frequent.v2 empty)
              #false)
(check-expect (most-frequent.v2 (list "Alabama"
                                      "Alaska"
                                      "Arkansas"
                                      "Louisiana"
                                      "Massachussetts"
                                      "Wyoming"))
              (make-lc "A" 3))

; Dictionary -> List-of-LetterCounts
; Count how often each letter is used as the
; first one of a word in the given dictionary d.
(define (count-by-letter d)
  (count-starting-letters (starting-letters d) d))

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
  (local
    (; Letter -> LetterCount
     ; Count how often ltr the starting
     ; letter in d is.
     (define (get-lc ltr)
       (make-lc ltr (starts-with# ltr d))))
    ; – IN –
    (map get-lc lol)))

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
  (local (; String -> Letter
          ; Return the first letter of str.
          (define (first-letter str)
            (if (> (string-length str) 0)
                (substring str 0 1)
                "")))
    ; – IN –
    (remove-duplicates (map first-letter d))))

(check-expect (starting-letters '())
              '())
(check-expect (starting-letters (list "Alabama"
                                      "arkansas"
                                      "apple"
                                      "banana"
                                      ""
                                      "orange"))
              (list "A" "a" "b" "" "o"))

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
(check-expect (remove-duplicates (list 1 2 3 4 5 6))
              (list 1 2 3 4 5 6))
(check-expect (remove-duplicates (list 1 1 2 2 2 3 3 3 4 4 4 4 5 5 5 5 6 6 6 6))
              (list 1 2 3 4 5 6))

; Letter Dictionary -> Number
; Count how many words in d start with l.
(define (starts-with# l d)
  (local (; String -> Boolean
          ; Does str start with l?
          (define (starts-with? str)
            (cond
              [(> (string-length str) 0)
               (string=? (substring str 0 1) l)]
              [else #false]))
          
          ; String -> Number
          ; Return 1 if str starts with l,
          ; 0 otherwise.
          ; Case-sensitive.
          (define (count-starts-with? str)
            (if (starts-with? str)
                1
                0)))
    ; – IN –
    (foldl (lambda (x y) (+ x y))
           0
           (map count-starts-with? d))))

(check-expect (starts-with# "a" '())
              0)
(check-expect (starts-with# "a" (list "Alabama"
                                      "arkansas"
                                      "ananas"
                                      "apple"
                                      "banana"
                                      ""
                                      "orange"))
              3)

; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(most-frequent AS-LIST)
(most-frequent.v2 AS-LIST)