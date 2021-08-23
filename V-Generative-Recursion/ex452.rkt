;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex452) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A File is one of: 
; – '()
; – (cons "\n" File)
; – (cons 1String File)
; Interpretation: represents the content of a file,
;                 "\n" is the newline character.

; A Line is a [List-of 1String].

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; File -> [List-of Line]
; Convert a file into a list of lines.
(define (file->list-of-lines afile)
  (cond
    [(empty? afile) '()]
    [else
     (cons (first-line afile)
           (file->list-of-lines (remove-first-line afile)))]))

(check-expect (file->list-of-lines '())
              '())
(check-expect (file->list-of-lines (list "\n"))
              (list '()))
(check-expect (file->list-of-lines (list "\n" "\n"))
              (list '() '()))
(check-expect (file->list-of-lines (list "a" "b" "c" "\n"
                                         "d" "e" "\n"
                                         "f" "g" "h" "\n"))
              (list (list "a" "b" "c")
                    (list "d" "e")
                    (list "f" "g" "h")))

; File -> Line
; Extract the first line from f.
(define (first-line f)
  (cond
    [(or (empty? f) (string=? (first f) "\n"))
     '()]
    [else
     (cons (first f) (first-line (rest f)))]))

(check-expect (first-line '())
              '())
(check-expect (first-line (list "\n"))
              '())
(check-expect (first-line (list "\n" "\n"))
              '())
(check-expect (first-line (list "a" "b" "c" "\n"
                                "d" "e" "\n"
                                "f" "g" "h" "\n"))
              (list "a" "b" "c"))

; File -> File
; Remove the first line from f.
(define (remove-first-line f)
  (cond
    [(empty? f) '()]
    [(string=? (first f) "\n")
     (rest f)]
    [else
     (remove-first-line (rest f))]))

(check-expect (remove-first-line '())
              '())
(check-expect (remove-first-line (list "\n"))
              '())
(check-expect (remove-first-line (list "\n" "\n"))
              (list "\n"))
(check-expect (remove-first-line (list "a" "b" "c" "\n"
                                       "d" "e" "\n"
                                       "f" "g" "h" "\n"))
              (list "d" "e" "\n"
                    "f" "g" "h" "\n"))
