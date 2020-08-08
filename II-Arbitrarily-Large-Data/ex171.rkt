;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex171) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define LINES (cons
               "TTT"
               (cons
                ""
                (cons
                 "Put up in a place"
                 (cons
                  "where it's easy to see"
                  (cons
                   "the cryptic admonishment"
                   (cons
                    "T.T.T."
                    (cons
                     ""
                     (cons
                      "When you feel how depressingly"
                      (cons
                       "slowly you climb,"
                       (cons
                        "it's well to remember that"
                        (cons
                         "Things Take Time."
                         (cons
                          ""
                          (cons "Piet Hein" '()))))))))))))))
                          

(define
  WORDS
  (cons
   "TTT"
   (cons
    "Put"
    (cons
     "up"
     (cons
      "in"
      (cons
       "a"
       (cons
        "place"
        (cons
         "where"
         (cons
          "it's"
          (cons
           "easy"
           (cons
            "to"
            (cons
             "see"
             (cons
              "the"
              (cons
               "cryptic"
               (cons
                "admonishment"
                (cons
                 "T.T.T."
                 (cons
                  "When"
                  (cons
                   "you"
                   (cons
                    "feel"
                    (cons
                     "how"
                     (cons
                      "depressingly"
                      (cons
                       "slowly"
                       (cons
                        "you"
                        (cons
                         "climb,"
                         (cons
                          "it's"
                          (cons
                           "well"
                           (cons
                            "to"
                            (cons
                             "remember"
                             (cons
                              "that"
                              (cons
                               "Things"
                               (cons
                                "Take"
                                (cons
                                 "Time."
                                 (cons
                                  "Piet"
                                  (cons
                                   "Hein"
                                   '()))))))))))))))))))))))))))))))))))

(define WORDS-LINES
  (cons
   (cons "TTT" '())
   (cons
    '()
    (cons
     (cons "Put" (cons "up" (cons "in" (cons "a" (cons "place" '())))))
     (cons
      (cons "where" (cons "it's" (cons "easy" (cons "to" (cons "see" '())))))
      (cons
       (cons "the" (cons "cryptic" (cons "admonishment" '())))
       (cons
        (cons "T.T.T." '())
        (cons
         '()
         (cons
          (cons "When"
                (cons "you"
                      (cons "feel"
                            (cons "how"
                                  (cons "depressingly" '())))))
          (cons
           (cons "slowly" (cons "you" (cons "climb," '())))
           (cons
            (cons "it's"
                  (cons "well"
                        (cons "to"
                              (cons "remember"
                                    (cons "that" '())))))
            (cons
             (cons "Things" (cons "Take" (cons "Time." '())))
             (cons
              '()
              (cons
               (cons "Piet" (cons "Hein" '()))
               '()))))))))))))))

; ------------------------------------------------------------------------------

(check-expect (read-lines "ttt.txt")
              LINES)

(check-expect (read-words "ttt.txt")
              WORDS)

(check-expect (read-words/line "ttt.txt")
              WORDS-LINES)