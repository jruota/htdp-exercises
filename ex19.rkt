;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex19) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Assume i is a number between 0 and the length of the given string (inclusive).
(define (string-insert str i)
  (cond
    [(= 0 (string-length str)) str]
    [(< 0 (string-length str))
     (string-append (substring str 0 i)
                    "_"
                    (substring str i))]))

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(string-insert "" 5)
(string-insert "helloworld" 5)