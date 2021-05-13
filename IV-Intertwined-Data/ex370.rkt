;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex370) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; An Xexpr.v2 is a list: 
; – (cons Symbol Body)
; – (cons Symbol (cons [List-of Attribute] Body))
; where Body is short for [List-of Xexpr.v2]

; An Attribute is a list of two items:
;   (cons Symbol (cons String '()))

; An XWord is '(word ((text String))).

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define WORD1 '(word ((text "banana"))))
(define WORD2 '(word ((text "apple"))))
(define WORD3 '(word ((text "orange"))))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Any -> Boolean
; Is w an XWord?
(define (word? w)
  (and
   ; determine limits of w, i.e. w has length of 2
   (cons? w)
   (cons? (rest w))
   (empty? (rest (rest w)))
   ; first element of w
   (symbol? (first w))
   (symbol=? (first w) 'word)
   ; second element of w
   ; limits
   (cons? (first (rest w)))    ; (list (list 'text String))
   (cons? (first (first (rest w))))    ; (list 'text String)
   (empty? (rest (first (rest w))))
   (empty? (rest (rest (first (first (rest w))))))
   ; content of the second element of w
   (symbol? (first (first (first (rest w)))))
   (symbol=? (first (first (first (rest w)))) 'text)
   (string? (first (rest (first (first (rest w))))))))

(check-expect (word? empty-image)
              #false)
(check-expect (word? 23)
              #false)
(check-expect (word? (make-posn 1 2))
              #false)
(check-expect (word? #true)
              #false)
(check-expect (word? (list 'word "word"))
              #false)
(check-expect (word? '())
              #false)
(check-expect (word? "word")
              #false)
(check-expect (word? '(word ((text 34))))
              #false)
(check-expect (word? '(word ((string->number "33"))))
              #false)
(check-expect (word? '(word (())))
              #false)
(check-expect (word? '(notaword ((text "word"))))
              #false)
(check-expect (word? '(word ((text "word") (text "word"))))
              #false)
(check-expect (word? '(word ((text "word" "word"))))
              #false)
(check-expect (word? '(word ((text "word")) ((text "word"))))
              #false)
(check-expect (word? '(word ((text "word")) word ((text "word"))))
              #false)
(check-expect (word? WORD1)
              #true)
(check-expect (word? WORD2)
              #true)
(check-expect (word? WORD3)
              #true)

; XWord -> String
; Extract the value of xw.
(define (word-text xw)
  (second (first (second xw))))

(check-expect (word-text WORD1)
              "banana")
(check-expect (word-text WORD2)
              "apple")
(check-expect (word-text WORD3)
              "orange")