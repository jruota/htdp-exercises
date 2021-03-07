;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex325) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define-struct no-info [])
(define NONE (make-no-info))
 
(define-struct node [ssn name left right])
; A BT (short for BinaryTree) is one of:
; – NONE
; – (make-node Number Symbol BT BT)

; A BST (short for binary search tree) is
; a BT according to the following conditions:
; – NONE is always a BST.
; – (make-node ssn0 name0 L R) is a BST if
;   – L is a BST,
;   – R is a BST,
;   – all ssn fields in L are smaller than ssn0,
;   – all ssn fields in R are larger than ssn0.

; A SymbolOrNone is one of:
; – NONE
; – Symbol
; Interpretation:
;     A Symbol or NONE.

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


(define NINE-A (make-node 99 'i NONE NONE))
(define EIGHT-A (make-node 24 'e NONE NONE))
(define SEVEN-A (make-node 10 'd NONE NONE))
(define SIX-A (make-node 95 'h NONE NINE-A))
(define FIVE-A (make-node 77 'g NONE NONE))
(define FOUR-A (make-node 15 'c SEVEN-A EIGHT-A))
(define THREE-A (make-node 89 'f FIVE-A SIX-A))
(define TWO-A (make-node 29 'b FOUR-A NONE))

(define TREE-A (make-node 63 'a TWO-A THREE-A))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; BT -> SymbolOrNone
; If the tree contains a node whose
; ssn field is n, produce the value
; of the name field in that node.
; Otherwise, produce NONE.
(define (search-bst bst n)
  (cond
    [(no-info? bst) NONE]
    [else
     (cond
       [(= (node-ssn bst) n)
        (node-name bst)]
       [else
        (if (> (node-ssn bst) n)
            (search-bst (node-left bst) n)
            (search-bst (node-right bst) n))])]))

(check-expect (search-bst NONE 77)
              NONE)
(check-expect (search-bst TREE-A 10)
              'd)
(check-expect (search-bst TREE-A 24)
              'e)
(check-expect (search-bst TREE-A 77)
              'g)
(check-expect (search-bst TREE-A 95)
              'h)
(check-expect (search-bst TREE-A 99)
              'i)

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; See exercise 189 for searching in sorted lists. Compare!

; Both functions first check for the empty version of their
; data type and return an appropriate value.

; In the next step they differ somewhat.

; The list searching function first checks whether there is an
; answer at all. It then checks the current list element and if
; it is not the relevant one, makes a recursive call to search
; the rest of the list.

; The function for the binary search tree first checks the current
; node, and if it is not the relevant one, chooses whether to search
; the right or the left tree.