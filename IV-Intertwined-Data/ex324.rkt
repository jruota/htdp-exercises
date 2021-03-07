;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex324) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

(define NINE (make-node 99 'i NONE NONE))
(define EIGHT (make-node 24 'e NONE NONE))
(define SEVEN (make-node 87 'd NONE NONE))
(define SIX (make-node 95 'h NONE NINE))
(define FIVE (make-node 33 'g NONE NONE))
(define FOUR (make-node 15 'c SEVEN EIGHT))
(define THREE (make-node 89 'f FIVE SIX))
(define TWO (make-node 29 'b FOUR NONE))

(define TREE-B (make-node 63 'a TWO THREE))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; BT -> [List-of Numbers]
; Produce the sequence of all the
; ssn numbers in the tree as they
; show up from left to right.
(define (inorder bt)
  (cond
    [(no-info? bt) '()]
    [else
     (append
      (inorder (node-left bt))
      (cons (node-ssn bt)
            (inorder (node-right bt))))]))

(check-expect (inorder NONE)
              '())
(check-expect (inorder TREE-A)
              (list 10 15 24 29 63 77 89 95 99))
(check-expect (inorder TREE-B)
              (list 87 15 24 29 63 33 89 95 99))

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; What does inorder produce for a binary search tree?

; A list of numbers sorted from least to greatest.
