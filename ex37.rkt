;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex37) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; String -> String
; Produce str with the first 1String removed.
(define (string-rest str)
  (if (zero? (string-length str))
      str
      (substring str 1)))

(string-rest "")
(string-rest "hello world")