;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex392) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define-struct branch [left right])
; A TOS is one of:
; – Symbol
; – (make-branch TOS TOS)
 
; A Direction is one of:
; – 'left
; – 'right
 
; A list of Directions is also called a path.

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define ERROR-MESSAGE "cannot traverse tree")

(define TOS25 (make-branch 'y 'z))
(define TOS24 (make-branch 'x TOS25))
(define TOS23 (make-branch 'w TOS24))
(define TOS22 (make-branch 'v TOS23))
(define TOS21 (make-branch 'u TOS22))
(define TOS20 (make-branch 't TOS21))
(define TOS19 (make-branch 's TOS20))
(define TOS18 (make-branch 'r TOS19))
(define TOS17 (make-branch 'q TOS18))
(define TOS16 (make-branch 'p TOS17))
(define TOS15 (make-branch 'o TOS16))
(define TOS14 (make-branch 'n TOS15))
(define TOS13 (make-branch 'm TOS14))
(define TOS12 (make-branch 'l TOS13))
(define TOS11 (make-branch 'k TOS12))
(define TOS10 (make-branch 'j TOS11))
(define TOS9 (make-branch 'i TOS10))
(define TOS8 (make-branch 'h TOS9))
(define TOS7 (make-branch 'g TOS8))
(define TOS6 (make-branch 'f TOS7))
(define TOS5 (make-branch 'e TOS6))
(define TOS4 (make-branch 'd TOS5))
(define TOS3 (make-branch 'c TOS4))
(define TOS2 (make-branch 'b TOS3))
(define TOS1 (make-branch 'a TOS2))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; TOS [List-of Direction] -> TOS
; Return the TOS lod leads to.
(define (pick-tree tos lod)
  (cond
    [(empty? lod) tos]
    [(symbol? tos) (error ERROR-MESSAGE)]
    [(branch? tos)
     (if (symbol=? (first lod) 'left)
         (pick-tree (branch-left tos) (rest lod))
         (pick-tree (branch-right tos) (rest lod)))]))

(check-expect (pick-tree 'symbol '())
              'symbol)
(check-error (pick-tree 'symbol (list 'left 'left 'left))
             ERROR-MESSAGE)
(check-expect (pick-tree TOS1 '())
              TOS1)
(check-expect (pick-tree TOS1 (list 'right 'right 'right 'right 'right 'left))
              'f)

(check-expect (pick-tree TOS14 (list 'right 'right 'right 'right 'right 'right))
              TOS20)