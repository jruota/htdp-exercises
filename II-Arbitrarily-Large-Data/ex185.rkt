;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex185) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(check-expect 1
              (first (list 1 2 3)))

(check-expect (list 2 3)
              (rest (list 1 2 3)))

(check-expect 2
              (second (list 1 2 3)))

; Find out from the documentation whether third and fourth exist.

; The function "first" through "tenth" exist.
