;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex120) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;1) (x)
;
; Illegal.
; "x" is a variable enclosed in parentheses but an open parentheses must be
; followed by one of the following:
;     – define,
;     – a primitive,
;     – a variable and at least one expression,
;     – a conditional expression.
;
;2) (+ 1 (not x))
;
; Legal.
; It is an expr of the form "(primitive expr expr)",
; where the second expr is of the form "(primitive expr)".
;
;3) (+ 1 2 3)
;
; Legal.
; It is an expr of the form "(primitive expr expr expr)".