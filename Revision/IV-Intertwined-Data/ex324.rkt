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

; BT -> [List-of Number]
; Produce the sequence of all the ssn numbers in the tree bt
; as they show up from left to right when looking at a tree drawing.
(define (inorder bt)
  (cond
    [(no-info? bt) '()]
    [else
     (local ((define left (inorder (node-left bt)))
             (define right (inorder (node-right bt))))
       ; – IN –
       (append left (list (node-ssn bt)) right))]))

(check-expect (inorder NONE) '())
(check-expect (inorder treeA) (list 10 15 24 29 63 77 89 95 99))
(check-expect (inorder treeB) (list 87 15 24 29 63 33 89 95 99))
