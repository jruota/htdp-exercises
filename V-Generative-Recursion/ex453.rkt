;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex453) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A File is one of: 
; – '()
; – (cons "\n" File)
; – (cons 1String File)
; Interpretation: represents the content of a file,
;                 "\n" is the newline character.

; A Line is a [List-of 1String].

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Line -> [List-of String]
; Turn a Line into a list of tokens.
; A token is either a 1String or a String that consists of
; lower-case letters and nothing else. That is, all white-space
; 1Strings are dropped; all other non-letters remain as is;
; and all consecutive letters are bundled into "words".
(define (tokenize l)
  (cond
    [(empty? l) '()]
    [else
     (cons (first-token l)
           (tokenize (remove-first-token l)))]))

(check-expect (tokenize '()) '())
(check-expect (tokenize (list "h" "o" "w" " "
                              "a" "r" "e" "\t"
                              "y" "o" "u" "?" "\n"))
              (list "how" "are" "you?"))

; Line -> String
; Return the first token, i.e. "word" from l.
(define (first-token l)
  (cond
    [(or (empty? l) (string-whitespace? (first l)))
     ""]
    [else
     (string-append (first l) (first-token (rest l)))]))

(check-expect (first-token '())
              "")
(check-expect (first-token (list "h" "o" "w" " "
                                 "a" "r" "e" "\t"
                                 "y" "o" "u" "?" "\n"))
              "how")

; Line -> Line
; Remove ther first token, i.e. word from l.
(define (remove-first-token l)
  (cond
    [(empty? l) '()]
    [(string-whitespace? (first l))
     (rest l)]
    [else
     (remove-first-token (rest l))]))

(check-expect (remove-first-token '())
              '())
(check-expect (remove-first-token (list "h" "o" "w" " "
                                        "a" "r" "e" "\t"
                                        "y" "o" "u" "?" "\n"))
              (list "a" "r" "e" "\t"
                    "y" "o" "u" "?" "\n"))