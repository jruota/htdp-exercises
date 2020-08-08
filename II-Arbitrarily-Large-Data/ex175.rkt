;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex175) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define-struct word-count [lines words letters])
; A WordCount is a structure:
;     (make-word-count Number Number Number)
; Interpretation:
;     The number of lines, words and letters in a
;     text file.

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; String -> WordCount
; Count the lines, words and letters in file f.
(define (wc f)
  (make-word-count
   (length (read-words/line f))
   (length (read-words f))
   (length (read-1strings f))))

(check-expect (wc "ttt.txt")
              (make-word-count 13 33 183))

; NOTE - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

; The result of the wc-function gives a different letter (byte) count than
; the wc utility on linux. This is probably because of a (missing) trailing
; newline at the end of the file.