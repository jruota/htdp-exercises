;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex326) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define-struct no-info [])
(define NONE (make-no-info))
 
(define-struct node [ssn name left right])
; A BT (short for BinaryTree) is one of:
; – NONE
; – (make-node Number Symbol BT BT)

; A BST (short for binary search tree) is a BT
; according to the following conditions:
; – NONE is always a BST.
; – (make-node ssn0 name0 L R) is a BST if
; – L is a BST,
; – R is a BST,
; – all ssn fields in L are smaller than ssn0,
; – all ssn fields in R are larger than ssn0.

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

(define NEW-Aa (make-node 88 'j NONE NONE))
(define FIVE-Aa (make-node 77 'g NONE NEW-Aa))
(define THREE-Aa (make-node 89 'f FIVE-Aa SIX-A))

(define TREE-Aa (make-node 63 'a TWO-A THREE-Aa))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; BST N Symbol -> BST
; Produce a BST that is just like bst and that
; in place of one NONE subtree contains the node
; structure (make-node n s NONE NONE).
(define (create-bst bst n s)
  (cond
    [(no-info? bst)
     (make-node n s NONE NONE)]
    [else
     (local ((define SSN (node-ssn bst)))
       (cond
         [(< n SSN)
          (make-node SSN
                     (node-name bst)
                     (create-bst (node-left bst) n s)
                     (node-right bst))]
         [(> n SSN)
          (make-node SSN
                     (node-name bst)
                     (node-left bst)
                     (create-bst (node-right bst) n s))]))]))

(check-expect (create-bst NONE 88 'j)
              (make-node 88 'j NONE NONE))
(check-expect (create-bst TREE-A 88 'j)
              TREE-Aa)
