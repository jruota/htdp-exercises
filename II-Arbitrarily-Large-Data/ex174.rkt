;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex174) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

; DATA DEFINTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A List-of-1Strings is one of:
; – '()
; – (cons 1String List-of-1String)
; Interpretation:
;     The collection of lists containing 1Strings.

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

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; String -> String
; Encode text files numerically. Each letter
; in a word will be encoded as a numeric
; three-letter string with a value between 0 and 256.
; The encoded file will be named like the input
; file with "encoded-" prepended.
(define (encode-file f)
  (write-file (string-append "encoded-" f)
              (collapse
               (encode-lolos
                (read-words/line f)))))

(check-expect (read-file (encode-file "hw.txt"))
              (string-append "104101108108111 119111114108100\n"
                             "104101108108111 119111114108100\n"
                             "104101108108111 119111114108100"))

; List-of-List-of-Strings -> List-of-List-of-Strings
; Encode all Strings in lolos as numeric strings.
(define (encode-lolos lolos)
  (cond
    [(empty? lolos) '()]
    [(cons? lolos)
     (cons (encode-los (first lolos))
           (encode-lolos (rest lolos)))]))

(check-expect (encode-lolos '())
              '())
(check-expect (encode-lolos (cons
                             (cons "hello" (cons "world" '()))
                             (cons
                              (cons "hello" (cons "world" '()))
                              '())))
              (cons
               (cons "104101108108111" (cons "119111114108100" '()))
               (cons
                (cons "104101108108111" (cons "119111114108100" '()))
                '())))

; List-of-Strings -> List-of-Strings
; Encode all strings in los as numeric strings.
(define (encode-los los)
  (cond
    [(empty? los) '()]
    [(cons? los)
     (cons (encode-string (first los))
           (encode-los (rest los)))]))

(check-expect (encode-los '())
              '())
(check-expect (encode-los (cons "hello" (cons "world" '())))
              (cons "104101108108111" (cons "119111114108100" '())))

; String -> String
; Encode s as a numeric string,
; where each letter is represented
; as a three-letter numeric string.
(define (encode-string s)
  (implode-los
   (encode-lo1s
    (explode s))))

(check-expect (encode-string "")
              "")
(check-expect (encode-string "hello")
              "104101108108111")

; List-of-1Strings -> List-of-Strings
; Encode every letter in lo1s as a
; three-letter numeric string.
(define (encode-lo1s lo1s)
  (cond
    [(empty? lo1s) '()]
    [(cons? lo1s)
     (cons (encode-letter (first lo1s))
           (encode-lo1s (rest lo1s)))]))

(check-expect (encode-lo1s '())
              '())
(check-expect (encode-lo1s (cons "h"
                                 (cons "e"
                                       (cons "l"
                                             (cons "l"
                                                   (cons "o" '()))))))
              (cons "104"
                    (cons "101"
                          (cons "108"
                                (cons "108"
                                      (cons "111" '()))))))

; List-of-Strings -> String
; Concatenate the strings in los into one string.
(define (implode-los los)
  (cond
    [(empty? los) ""]
    [(cons? los)
     (string-append (first los)
                    (implode-los (rest los)))]))

(check-expect (implode-los '())
              "")
(check-expect (implode-los (cons "h"
                                 (cons "e"
                                       (cons "l"
                                             (cons "l"
                                                   (cons "o" '()))))))
              "hello")

; given functions - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

; The code1 function does the actual conversion from 1String to numeric string,
; while encode-letter makes sure that these numeric strings are three
; "letters" long.
; The ordering of the conditional clauses in encode-letter matters, although
; one could use
;     (and (< (string->int s) 100)
;          (> (string->int s) 10))
; so that it would not matter anymore.

; 1String -> String
; converts the given 1String to a 3-letter numeric String
(define (encode-letter s)
  (cond
    [(>= (string->int s) 100) (code1 s)]
    [(< (string->int s) 10)
     (string-append "00" (code1 s))]
    [(< (string->int s) 100)
     (string-append "0" (code1 s))]))

(check-expect (encode-letter "z") (code1 "z"))
(check-expect (encode-letter "\t")
              (string-append "00" (code1 "\t")))
(check-expect (encode-letter "a")
              (string-append "0" (code1 "a")))
 
; 1String -> String
; converts the given 1String into a String
(define (code1 c)
  (number->string (string->int c)))

(check-expect (code1 "z") "122")

; from ex172.rkt (Exercise 172) - - - - - - - - - - - - - - - - - - - - - - - -

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

; String -> String
; Remove the trailing whitespace (" ", "\n", "\t" "\r")
; from s.
(define (remove-trailing-whitespace s)
  (cond
    [(or (string=? (string-last s) " ")
         (string=? (string-last s) "\n")
         (string=? (string-last s) "\t")
         (string=? (string-last s) "\r"))
     (remove-trailing-whitespace (substring s 0 (sub1 (string-length s))))]
    [else s]))

; String -> 1String
; Return the last character of s.
(define (string-last s)
  (cond
    [(> (string-length s) 0)
     (substring s (sub1 (string-length s)))]
    [else s]))

; ------------------------------------------------------------------------------

(encode-file "ttt.txt")