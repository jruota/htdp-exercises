;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex116) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;1) x
;
; "x" is a variable.
; [variable = expr = def-expr = program)
;
;2) (= y z)
;
; This is a function call.
; [(primitive expr expr) = expr = def-expr = program]
;
;3) (= (= y z) 0)
;
; This is a function call nested within a function call.
; [(primitive (primitive expr expr) value) = expr = def-expr = program]