;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex190-alt) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; This version was designed after an email with questions about the exercise was
; answered by one of the authors and contained some hints.

; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A List-of-1Strings is one of:
; – '()
; – (cons 1String List-of-1Strings)
; Interpretation:
;     A list containing letters.

; A List-of-List-of-1Strings is one of:
; – '()
; – (cons List-of-1Strings List-of-List-of-1Strings)
; Interpretation:
;     A list containing lists of letters.

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; List-of-1Strings -> List-of-List-of-1Strings
; Produce the list of all prefixes. A list p is
; a prefix of l if p and l are the same up through
; all items in p.
(define (prefix lo1s)
  (cond
    [(empty? lo1s) '()]
    [(cons? lo1s)
     (cons
      (list (first lo1s))
      (add-to-front (first lo1s) (prefix (rest lo1s))))]))

(check-expect (prefix '())
              '())
(check-expect (prefix (list "a" "b" "c" "d"))
              (list (list "a")
                    (list "a" "b")
                    (list "a" "b" "c")
                    (list "a" "b" "c" "d")))

; List-of-1Strings -> List-of-List-of-1Strings
; Produce the list of all suffixes. A list p is
; a suffix of l if p and l are the same from the
; end, up through all items in p.
(define (suffix lo1s)
  (cond
    [(empty? lo1s) '()]
    [(cons? lo1s)
     (cons
      lo1s
      (suffix (rest lo1s)))]))

(check-expect (suffix '())
              '())
(check-expect (suffix (list "a" "b" "c" "d"))
              (list (list "a" "b" "c" "d")
                    (list "b" "c" "d")
                    (list "c" "d")
                    (list "d")))

; 1String List-of-List-of-1Strings -> List-of-List-of-1Strings
; Add s to the beginning of all elements
; of lolo1s.
(define (add-to-front s lolo1s)
  (cond
    [(empty? lolo1s) '()]
    [(cons? lolo1s)
     (cons
      (cons s (first lolo1s))
      (add-to-front s (rest lolo1s)))]))

(check-expect (add-to-front "a" '())
              '())
(check-expect (add-to-front "a" (list (list "d")))
              (list (list "a" "d")))
(check-expect (add-to-front "a" (list (list "b")
                                      (list "b" "c")))
              (list (list "a" "b")
                    (list "a" "b" "c")))
