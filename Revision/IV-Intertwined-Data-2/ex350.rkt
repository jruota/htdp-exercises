;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex350) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; What is unusual about the definition of this program with respect to the
; design recipe?

; The function parse-sl deals with SL's and therefore its basic structure
; should be
;
;     ...
;     (cond
;       [(empty? s) ...]
;       [else ...])
;     ...
;
; but instead it checks whether its argument is three elements long or not.