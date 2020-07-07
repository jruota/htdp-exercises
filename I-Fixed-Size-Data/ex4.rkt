;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define str "helloworld")
(define i 5)

(string-append (substring str 0 i)
               (substring str (+ i 1)))

; Which values for i are legitimate?

; If one were to use i directly, the valid range for i is 0 to 10,
; i.e. up to and including the length of the string.
; The way i is used here ("(+ i 1)") 0 to 9 is the valid range.