;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex117) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;1) (3 + 4)
;
; An open parentheses must be followed by a primitive, a variable or a cond,
; none of which is the case here.
;
;2) number?
;
; "number?" is a primitive and primitives have to be enclosed in parentheses
; and be directly followed by at least one expression.
;
;3) (x)
;
; An open parentheses must be followed by a primitive, a variable or a cond,
; none of which is the case here, since x is a variable.