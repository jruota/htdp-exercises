;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex137) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A List-of-strings is one of:
; – '()
; – (cons String List-of-strings)
; Interpretation:
;     The collection of all lists containing Strings only.

; A List-of-names is one of: 
; – '()
; – (cons String List-of-names)
; interpretation a list of invitees, by last name

; List-of-strings -> Number
; determines how many strings are on alos
; List-of-strings -> Number
; determines how many strings are on alos
(define (how-many alos)
  (cond
    [(empty? alos) ...]
    [else
     (... (first alos) ...
      ... (how-many (rest alos)) ...)]))

; List-of-names -> Boolean
; determines whether "Flatt" occurs on alon
(define (contains-flatt? alon)
  (cond
    [(empty? alon) ...]
    [(cons? alon)
     (... (first alon) ... (rest alon) ...)]))

(check-expect (contains-flatt? (cons "X" (cons "Y"  (cons "Z" '()))))
              #false)
(check-expect (contains-flatt? (cons "A" (cons "Flatt" (cons "C" '()))))
              #true)

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Compare the template for contains-flatt? with the one for how-many.
; Ignoring the function name, they are the same. Explain the similarity.

; List-of-strings and List-of-names are practically the same data definitions
; in that they define lists of strings. Thus any function template dealing with
; either List-of-strings or List-of-names must be equal. It is only in the later
; design steps that the function, depending on their purpose, will start to
; differ.