;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex363) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; An Xexpr.v2 is a list: 
; – (cons Symbol Body)
; – (cons Symbol (cons [List-of Attribute] Body))
; where Body is short for [List-of Xexpr.v2]

; An Attribute is a list of two items:
;   (cons Symbol (cons String '()))

; ---------------------------------------------

; An Xexpr.v3 is a list:
; – (cons Symbol Rest)
; Interpretation:
;     Representation of XML as S-expressions.

; Rest can be one of:
; – (cons Attributes Body)
; – Body
; Interpretation:
;     Representation of Attributes of XML elements
;     and further XML elements.

; Attributes is a [List-of Attribute].

; Body is a [List-of Xexpr.v3].

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define one (cons 'one '()))
(define two (cons 'two '()))
(define three (cons 'three '()))

(define body (list one two three))

(define attr-one (cons 'attr (cons "one" '())))
(define attr-two (cons 'attr (cons "two" '())))
(define attr-three (cons 'attr (cons "three" '())))

(define attr-list (list attr-one attr-two attr-three))

(define Rest (cons attr-list body))

(cons 'symbol body)
(cons 'symbol Rest)
