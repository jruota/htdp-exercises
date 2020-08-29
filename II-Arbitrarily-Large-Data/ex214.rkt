;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex214) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

; String -> List-of-strings
; finds all words that the letters of some given word spell
(define (alternative-words s)
  (in-dictionary
    (words->strings (arrangements (string->word s)))))

(check-member-of (alternative-words "cat")
                 (list "act" "cat")
                 (list "cat" "act"))
 
(check-satisfied (alternative-words "rat")
                 all-words-from-rat?)

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

; Word -> List-of-words
; creates all rearrangements of the letters in w
(define (arrangements w)
  (cond
    [(empty? w) (list '())]
    [else (insert-everywhere/in-all-words (first w)
            (arrangements (rest w)))]))

(check-expect (arrangements '())
              (list '()))
(check-expect (arrangements (explode "rat"))
              (list (list "r" "a" "t")
                    (list "a" "r" "t")
                    (list "a" "t" "r")
                    (list "r" "t" "a")
                    (list "t" "r" "a")
                    (list "t" "a" "r")))

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

; 1String List-of-words -> List-of-words
; Return low with l inserted at the beginning,
; between all letters, and at the end of all words
; of low.
(define (insert-everywhere/in-all-words l low)
  (cond
    [(empty? low) '()]
    [(cons? low)
     (append (insert-everywhere/in-word l (first low))
             (insert-everywhere/in-all-words l (rest low)))]))

(check-expect (insert-everywhere/in-all-words "a" '())
              '())
(check-expect (insert-everywhere/in-all-words "a" (list (list "b")))
              (list (list "a" "b")
                    (list "b" "a")))
(check-expect (insert-everywhere/in-all-words "d"
                                              (list (list "e" "r")
                                                    (list "r" "e")))
              (list (list "d" "e" "r")
                    (list "e" "d" "r")
                    (list "e" "r" "d")
                    (list "d" "r" "e")
                    (list "r" "d" "e")
                    (list "r" "e" "d")))

; 1String Word -> List-of-words
; Return a list of words with l inserted
; at the beginning, between all letters, and
; at the end of w.
(define (insert-everywhere/in-word l w)
  (cond
    [(empty? w) (list (list l))]
    [(cons? w)
     (append (list (cons l w))
             (insert-at-the-beginning/in-word
              (first w)
              (insert-everywhere/in-word
               l
               (rest w))))]))

(check-expect (insert-everywhere/in-word "a" '())
              (list (list "a")))
(check-expect (insert-everywhere/in-word "a" (list "b"))
              (list (list "a" "b")
                    (list "b" "a")))
(check-expect (insert-everywhere/in-word "a" (list "b" "c"))
              (list (list "a" "b" "c")
                    (list "b" "a" "c")
                    (list "b" "c" "a")))

; 1String List-of-words -> List-of-words
; Return a list of words with l inserted
; at the beginning of every element in low.
(define (insert-at-the-beginning/in-word l low)
  (cond
    [(empty? low) '()]
    [(cons? low)
     (cons (cons l (first low))
           (insert-at-the-beginning/in-word l (rest low)))]))

(check-expect (insert-at-the-beginning/in-word "a" '())
              '())
(check-expect (insert-at-the-beginning/in-word "a" (list (list "b")))
              (list (list "a" "b")))
(check-expect (insert-at-the-beginning/in-word "a" (list (list "b")
                                                         (list "c")))
              (list (list "a" "b")
                    (list "a" "c")))

; List-of-strings -> Boolean
; For testing purposes only.
(define (all-words-from-rat? w)
  (and
    (member? "rat" w) (member? "art" w) (member? "tar" w)))