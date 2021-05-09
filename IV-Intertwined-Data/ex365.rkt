;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex365) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; An Xexpr is a list: 
; – (cons Symbol Body)
; – (cons Symbol (cons [List-of Attribute] Body))
; where Body is short for [List-of Xexpr]

; An Attribute is a list of two items:
;   (cons Symbol (cons String '()))

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; 1. ---------------------------------------------------------------------------

'(server ((name "example.org")))
; <server name="example.org"></server>

; 2. ---------------------------------------------------------------------------

'(carcas (board (grass)) (player ((name "sam"))))
; <carcas>
;   <board>
;     <grass></grass>
;   </board>
;   <player name="sam"></player>
; </carcas>

; 3. ---------------------------------------------------------------------------

'(start)
; <start></start>

; ------------------------------------------------------------------------------

; Number 3 could be represented in Xexpr.v0, because it is just a single item
; with no attributes. It is also the only one that could be represented in
; Xexpr.v1 (where the "[List-of Xexpr.v1]" part would be the empty list), as
; numbers 1 and 2 have attributes that cannot be represented in Xexpr.v1.