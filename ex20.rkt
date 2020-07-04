;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex20) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Assume i is a number between 0 (inclusive)
; and the length of the given string (exclusive).

(define (string-delete str i)
  (cond
    [(= 0 (string-length str)) str]
    [(> (string-length str) 0)
     (string-append (substring str 0 i)
                    (substring str (+ i 1)))]))

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(string-delete "" 5)
(string-delete "hello world" 5)