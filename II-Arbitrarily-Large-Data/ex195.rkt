;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex195) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A Letter is one of the following 1Strings: 
; – "a"
; – ... 
; – "z"
; or, equivalently, a member? of this list: 
(define LETTERS
  (explode "abcdefghijklmnopqrstuvwxyz"))

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define LOCATION "/usr/share/dict/words")

; A Dictionary is a List-of-strings.
(define AS-LIST (read-lines LOCATION))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Letter Dictionary -> Number
; Count how many words in d start with l.
(define (starts-with# l d)
  (cond
    [(empty? d) 0]
    [(cons? d)
     (if (starts-with? l (first d))
         (+ 1 (starts-with# l (rest d)))
         (starts-with# l (rest d)))]))

(check-expect (starts-with# "a" '())
              0)
(check-expect (starts-with# "a" (list"Andalusia"
                                     "alternative"
                                     "amicabliy"
                                     "ant"
                                     "apple"
                                     "boar"
                                     "gorilla"
                                     "zebra"))
              4)
(check-expect (starts-with# "A" (list "Andalusia"
                                      "alternative"
                                      "amicabliy"
                                      "ant"
                                      "apple"
                                      "boar"
                                      "gorilla"
                                      "zebra"))
              1)

; Letter String -> Boolean
; Does s start with l?
(define (starts-with? l s)
  (cond
    [(< 0 (string-length s))
     (string=? l (substring s 0 1))]
    [else #false]))

(check-expect (starts-with? "a" "")
              #false)
(check-expect (starts-with? "A" "alabama")
              #false)
(check-expect (starts-with? "a" "alabama")
              #true)
(check-expect (starts-with? "A" "Alabama")
              #true)

; ------------------------------------------------------------------------------

(length AS-LIST)

(starts-with# "e" AS-LIST)
(starts-with# "z" AS-LIST)