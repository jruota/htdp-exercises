;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex79) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A Color is one of: 
; — "white"
; — "yellow"
; — "orange"
; — "green"
; — "red"
; — "blue"
; — "black"

; Examples:
;    – "orange"
;    – "red"
;    – "black"

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; H is a Number between 0 and 100.
; interpretation represents a happiness value

; Examples:
;    – 48
;    – 74
;    – 19

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define-struct person [fstname lstname male?])
; A Person is a structure:
;   (make-person String String Boolean)

; Examples:
;    – (make-person "Abby" "Anderson" #false)
;    – (make-person "Joel" "Miller" #true)
;    – (make-person "Karl" "Dieringer" #true)

; Is it a good idea to use a field name that looks like the name of a predicate?

; Appending a question mark to the field name may hint to it containing a
; boolean value. On the other hand, one might confuse it with a predicate,
; although it would always be used with the structure name prepended, i.e.
; person-male?. It is a mixed bag and a question of personal preference.

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define-struct dog [owner name age happiness])
; A Dog is a structure:
;   (make-dog Person String PositiveInteger H)
; Interpretation:
;     Combines the owner of the dog with its name, age
;     and happiness level.

; Examples:
;    – (make-dog (make-person "Marek" "Kowalski" #true)
;                "Dora" 6 76)


; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A Weapon is one of: 
; — #false
; — Posn
; interpretation #false means the missile hasn't 
; been fired yet; a Posn means it is in flight

; Examples:
;    – #false
;    – (make-posn 55 14)
;    – (make-posn 60 2)