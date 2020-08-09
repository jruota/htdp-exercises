;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex173) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; String -> String
; Remove all articles (a, an, the) from the
; text in f and write it to a file with the
; the file name of f prepended with
; "no-articles-".
(define (remove-articles f)
  (write-file (string-append "no-articles-" f)
              (collapse
               (delete-articles (read-words/line f)))))

; Only tests the creation and naming of a file. To test the content of the file
; use the diff-utility on Linux.
(check-expect (remove-articles "test.txt")
              "no-articles-test.txt")

; List-of-List-of-Strings -> List-of-List-of-Strings
; Remove all articles in lolos.
(define (delete-articles lolos)
  (cond
    [(empty? lolos) '()]
    [(cons? lolos)
     (cons (no-articles (first lolos))
           (delete-articles (rest lolos)))]))

(check-expect (delete-articles '())
              '())
(check-expect (delete-articles
               (cons
                (cons "The"
                      (cons "pine"
                            (cons "is"
                                  (cons "a"
                                        (cons "tree." '())))))
                (cons
                 (cons "A"
                       (cons "rock"
                             (cons "is"
                                   (cons "not"
                                         (cons "a"
                                               (cons "tree." '()))))))
                 '())))
              (cons
               (cons "pine" (cons "is" (cons "tree." '())))
               (cons
                (cons "rock" (cons "is" (cons "not" (cons "tree." '()))))
                '())))
              

; List-of-Strings -> List-of-Strings
; Remove all articles in los.
(define (no-articles los)
  (cond
    [(empty? los) '()]
    [(cons? los)
     (if (article? (first los))
         (no-articles (rest los))
         (cons (first los)
               (no-articles (rest los))))]))

(check-expect (no-articles '())
              '())
(check-expect (no-articles
               (cons
                "The"
                (cons
                 "apple"
                 (cons
                  "is"
                  (cons
                   "a"
                   (cons
                    "fruit"
                    (cons
                     "of"
                     (cons
                      "a"
                      (cons
                       "round"
                       (cons
                        "shape."
                        '()))))))))))
              (cons "apple"
                    (cons "is"
                          (cons "fruit"
                                (cons "of"
                                      (cons "round"
                                            (cons "shape."
                                                  '())))))))

; String -> Boolean
; Is s an article?
(define (article? s)
  (or (string=? (string-downcase s) "a")
      (string=? (string-downcase s) "an")
      (string=? (string-downcase s) "the")))

(check-expect (article? "a")
              #true)
(check-expect (article? "an")
              #true)
(check-expect (article? "the")
              #true)
(check-expect (article? "A")
              #true)
(check-expect (article? "An")
              #true)
(check-expect (article? "The")
              #true)
(check-expect (article? "hello")
              #false)

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

(remove-articles "ttt.txt")

(remove-articles "test.txt")
(check-expect (read-file "no-articles-test.txt")
              (read-file "test-without-articles.txt"))