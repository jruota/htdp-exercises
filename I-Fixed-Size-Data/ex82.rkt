;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex82) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define-struct tlw [one two three])
; A TLW (three letter word) is a structure:
;     (make-tlw 1SoF 1SoF 1SoF)
; Interpretation:
;     All lowercase, three letter words.

; A 1SoF (1String or #false) is one of:
;     – the lowercase letters "a" through "z"
;     – #false
; Interpretation:
;     Represents a letter.

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; TLW TLW -> TLW
; Produce a word that indicates where the give ones
; agree and disagree.
(define (compare-word tlw1 tlw2)
  (make-tlw (compare-letter (tlw-one tlw1) (tlw-one tlw2))
            (compare-letter (tlw-two tlw1) (tlw-two tlw2))
            (compare-letter (tlw-three tlw1) (tlw-three tlw2))))

(check-expect (compare-word (make-tlw "r" "e" "d") (make-tlw "t" "o" "p"))
              (make-tlw #false #false #false))
(check-expect (compare-word (make-tlw "d" "e" "r") (make-tlw "d" "i" "e"))
              (make-tlw "d" #false #false))
(check-expect (compare-word (make-tlw "o" "k" "o") (make-tlw "o" "k" "o"))
              (make-tlw "o" "k" "o"))

; 1String -> 1SoF
; Compare the given letters, returning
; a letter if they are the same and #false
; otherwise.
(define (compare-letter l1 l2)
  (if (string=? l1 l2)
      l1
      #false))

(check-expect (compare-letter "a" "b")
              #false)
(check-expect (compare-letter "a" "a")
              "a")