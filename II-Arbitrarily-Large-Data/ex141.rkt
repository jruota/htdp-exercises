;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex141) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A List-of-string is one of:
; – '()
; – (cons String List-of-string)
; Interpretation:
;     The collection of all lists containing Strings.

; List-of-string -> String
; concatenates all strings in l into one long string
(define (cat l)
  (cond
    [(empty? l) ""]
    [else
     (string-append (first l) (cat (rest l)))]))

(check-expect (cat '())
              "")
(check-expect (cat (cons "a" (cons "b" '())))
              "ab")
(check-expect (cat (cons "ab" (cons "cd" (cons "ef" '()))))
              "abcdef")

(cat (cons "a" '()))