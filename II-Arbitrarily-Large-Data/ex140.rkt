;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex140) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITION ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A List-of-Booleans is one of:
; – '()
; – (cons Boolean List-of-Booleans)
; Interpretation:
;     The collection of all lists containing
;     Booleans only.

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; List-of-Booleans -> Boolean
; Determine whether all elements
; of lob are #true.
(define (all-true lob)
  (cond
    [(empty? lob) #true]
    [(cons? lob)
     (and (first lob)
          (all-true (rest lob)))]))

(check-expect (all-true '())
              #true)
(check-expect (all-true (cons #true '()))
              #true)
(check-expect (all-true (cons #false '()))
              #false)
(check-expect (all-true (cons #true (cons #false '())))
              #false)
(check-expect (all-true (cons #true (cons #true (cons #true '()))))
              #true)
(check-expect (all-true (cons #true (cons #true (cons #false '()))))
              #false)

; NOTE - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; The answer for empty lists (#false) is dictated by the answer for lists
; that contain #false values only. Or short-circuits as soon as it finds
; a #true, then the answer becomes #true. But for lists containing #false
; only, the answer needs to be false.
; END NOTE - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; List-of-Booleans -> Boolean
; Determine whether at least one
; element of lob is #true.
(define (one-true lob)
  (cond
    [(empty? lob) #false]
    [(cons? lob)
     (or (first lob)
         (one-true (rest lob)))]))

(check-expect (one-true '())
              #false)
(check-expect (one-true (cons #true '()))
              #true)
(check-expect (one-true (cons #false '()))
              #false)
(check-expect (one-true (cons #true (cons #false '())))
              #true)
(check-expect (one-true (cons #false
                              (cons #false
                                    (cons #false
                                          (cons #false
                                                (cons #true '()))))))
              #true)
(check-expect (one-true (cons #false
                              (cons #false
                                    (cons #false
                                          (cons #false
                                                (cons #false '()))))))
              #false)