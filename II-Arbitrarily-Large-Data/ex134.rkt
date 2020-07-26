;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex134) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITION ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A List-of-Strings is one of:
; – '()
; – (cons String List-of-Strings)
; Interpretation:
;     The collection of all lists containing
;     solely strings.

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; List-of-Strings -> Boolean
; Determine whether s occurs on los.
(define (contains? s los)
  (cond
    [(empty? los) #false]
    [(cons? los)
     (or (string=? (first los) s)
         (contains? s (rest los)))]))

(check-expect (contains? "Flatt" '())
              #false)
(check-expect (contains? "Flatt" (cons "Nope"
                                       (cons "Njet"
                                             (cons "Nie"
                                                   (cons "Flatt" '())))))
              #true)