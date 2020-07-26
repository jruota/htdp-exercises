;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex125) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; The grammar for defining structures is as follows:
;
;     (define-struct name [name ...]).
;
; Any legal structure type definition must follow this grammar.

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define-struct oops [])

; Legal, because open-parenthesis is followed by the "define-struct" keyword,
; a name and brackets with zero or more names ("..." means zero or more).

(define-struct child [parents dob date])

; Legal, because open-parenthesis is followed by the "define-struct" keyword,
; a name and brackets with three names.

(define-struct (child person) [dob date])

; Illegal, because the "define-struct" keyword is followed by two names enclosed
; in parentheses where only a name would be legal.