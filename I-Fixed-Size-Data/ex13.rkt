;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex13) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define (string-first str)
  (if (> (string-length str) 0)
      (substring str 0 1)
      ""))

(string-first "hello world")
(string-first "hello")
(string-first "world")
(string-first "")