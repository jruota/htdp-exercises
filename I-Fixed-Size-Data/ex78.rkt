;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex78) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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