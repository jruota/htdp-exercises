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

; A BST (short for binary search tree) is a BT
; according to the following conditions:
; – NONE is always a BST
; – (make-node ssn0 name0 L R) is a BST if
;       - L is a BST,
;       - R is a BST,
;       - all ssn fields in L are smaller than ssn0,
;       - all ssn fields in R are larger than ssn0.

; A MaybeSymbol is one of:
; – Symbol
; – #false

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define node10a (make-node 10 'a NONE NONE))
(define node24a (make-node 24 'b NONE NONE))
(define node77a (make-node 77 'c NONE NONE))
(define node99a (make-node 99 'd NONE NONE))
(define node15a (make-node 15 'e node10a node24a))
(define node95a (make-node 95 'f NONE node99a))
(define node29a (make-node 29 'g node15a NONE))
(define node89a (make-node 89 'h node77a node95a))
(define node63a (make-node 63 'i node29a node89a))

(define node87b (make-node 87 'a NONE NONE))
(define node24b (make-node 24 'b NONE NONE))
(define node33b (make-node 33 'c NONE NONE))
(define node99b (make-node 99 'd NONE NONE))
(define node15b (make-node 15 'e node87b node24b))
(define node95b (make-node 95 'f NONE node99b))
(define node29b (make-node 29 'g node15b NONE))
(define node89b (make-node 89 'h node33b node95b))
(define node63b (make-node 63 'i node29b node89b))

(define treeA node63a)
(define treeB node63b)

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Number BST -> Symbol
; If the binary search tree bst contains a node
; whose ssn field is n, the function produces the
; value of the name field in that node.
; Otherwise, the function produces NONE.
(define (search-bst n bst)
  (cond
    [(no-info? bst) NONE]
    [else
     (local ((define ssn (node-ssn bst)))
       ; – IN –
       (cond
         [(= ssn n)
          (node-name bst)]
         [(< n ssn)
          (search-bst n (node-left bst))]
         [(> n ssn)
          (search-bst n (node-right bst))]))]))

(check-expect (search-bst 99 NONE) NONE)
(check-expect (search-bst 99 treeA) 'd)
(check-expect (search-bst 24 treeA) 'b)

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(search-bst 87 treeB) ; fails
