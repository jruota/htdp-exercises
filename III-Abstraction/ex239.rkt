;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex239) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A [List X Y] is a list of lenght 2: 
;     (cons X (cons Y '()))

; A Pair-of-Numbers is a list:
;     [List Number Number]

; A PoN1S (pair of a Number and a 1String) is a list:
;     [List Number 1String]

; A PoNB (pair of a Number and a Boolean) is a list:
;     [List Number Boolean]

; DATA EXAMPLES ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Pair-of-Numbers
(cons 2 (cons 17 '()))

; PoN1S
(cons 2 (cons "f" '()))

; PoNB
(cons 2 (cons #true '()))