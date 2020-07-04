;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex38) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; String -> String
; Return str with the last 1String removed.
(define (string-remove-last str)
  (if (zero? (string-length str))
      str
      (substring str (- (string-length str) 1) (string-length str))))

(string-remove-last "")
(string-remove-last "hello world")