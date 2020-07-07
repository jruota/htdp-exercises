;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex34) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; String -> 1String
; Extract the first 1String from str if str has a length
; greater than 0, return the empty string otherwise.

(define (string-first str)
  (cond
    [(zero? (string-length str)) str]
    [(> (string-length str) 0)
     (substring str 0 1)]))

(string-first "hello world")
(string-first "")