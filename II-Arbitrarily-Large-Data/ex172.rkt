;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex172) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

; DATA DEFINTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A List-of-Strings is one of:
; – '()
; – (cons String List-of-Strings)
; Interpretation:
;     The collection of all lists containing Strings.

; A List-of-List-of-Strings is one of:
; – '()
; – (cons List-of-Strings List-of-List-of-Strings)
; Interpretation:
;     The collection of all lists containing
;     List-of-Strings.

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; List-of-List-of-Strings -> String
; Convert a list of lines into one string,
; separating the lines by newlines and
; the strings by spaces.
(define (collapse lolos)
  (cond
    [(empty? lolos) ""]
    [(cons? lolos)
     (remove-trailing-whitespace
      (string-append (collapse-words (first lolos))
                     "\n"
                     (collapse (rest lolos))))]))

(check-expect (collapse '())
              "")
(check-expect (collapse
               (cons
                (cons
                 "Out"
                 (cons
                  "of"
                  (cons
                   "the"
                   (cons
                    "night"
                    (cons
                     "that"
                     (cons
                      "covers"
                      (cons
                       "me" '())))))))
                (cons
                 (cons
                  "Black"
                  (cons
                   "as"
                   (cons
                    "the"
                    (cons
                     "pit"
                     (cons
                      "from"
                      (cons
                       "pole"
                       (cons
                        "to"
                        (cons
                         "pole," '()))))))))
                 (cons
                  (cons
                   "I"
                   (cons
                    "thank"
                    (cons
                     "whatever"
                     (cons
                      "gods"
                      (cons
                       "may"
                       (cons
                        "be" '()))))))
                  (cons
                   (cons
                    "For"
                    (cons
                     "my"
                     (cons
                      "unconquerable"
                      (cons
                       "soul." '()))))
                   '())))))
              (string-append
               "Out of the night that covers me\n"
               "Black as the pit from pole to pole,\n"
               "I thank whatever gods may be\n"
               "For my unconquerable soul."))

; List-of-Strings -> String
; Convert a list of strings into one string,
; separating the strings with spaces.
(define (collapse-words los)
  (cond
    [(empty? los) ""]
    [(cons? los)
     (remove-trailing-whitespace
      (string-append (first los)
                     " "
                     (collapse-words (rest los))))]))

(check-expect (collapse-words '())
              "")
(check-expect (collapse-words
               (cons "In"
                     (cons "the"
                           (cons "fell"
                                 (cons "clutch"
                                       (cons "of"
                                             (cons "circumstance," '())))))))
              "In the fell clutch of circumstance,")

; String -> String
; Remove the trailing whitespace (" ", "\n", "\t")
; from s.
(define (remove-trailing-whitespace s)
  (cond
    [(or (string=? (string-last s) " ")
         (string=? (string-last s) "\n")
         (string=? (string-last s) "\t"))
     (remove-trailing-whitespace (substring s 0 (sub1 (string-length s))))]
    [else s]))

(check-expect (remove-trailing-whitespace "hello world")
              "hello world")
(check-expect (remove-trailing-whitespace "hello world    ")
              "hello world")
(check-expect (remove-trailing-whitespace "hello world\n\n\n")
              "hello world")
(check-expect (remove-trailing-whitespace "hello world\t\t")
              "hello world")
(check-expect (remove-trailing-whitespace "hello world \t  \n")
              "hello world")

; String -> 1String
; Return the last character of s.
(define (string-last s)
  (cond
    [(> (string-length s) 0)
     (substring s (sub1 (string-length s)))]
    [else s]))

(check-expect (string-last "")
              "")
(check-expect (string-last "hello")
              "o")

; CHALLENGE ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Use the diff utility on Linux to compare the two files.
(write-file "ttt.dat" (collapse (read-words/line "ttt.txt")))