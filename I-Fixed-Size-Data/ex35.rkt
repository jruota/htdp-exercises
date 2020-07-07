;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex35) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; String -> 1String
; Exctract the last 1String from a non-empty String.
(define (string-last str)
  (substring str (- (string-length str) 1) (string-length str)))

(string-last "h")
(string-last "hello world")