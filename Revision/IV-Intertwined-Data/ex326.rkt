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

(define ERR-MSG "social security number already exists")

(define node10a (make-node 10 'a NONE NONE))
(define node24a (make-node 24 'b NONE NONE))
(define node77a (make-node 77 'c NONE NONE))
(define node99a (make-node 99 'd NONE NONE))
(define node15a (make-node 15 'e node10a node24a))
(define node95a (make-node 95 'f NONE node99a))
(define node29a (make-node 29 'g node15a NONE))
(define node89a (make-node 89 'h node77a node95a))
(define node63a (make-node 63 'i node29a node89a))

(define node10new (make-node 10 'a NONE NONE))
(define node24new (make-node 24 'b NONE NONE))
(define node77new (make-node 77 'c NONE NONE))
(define node123new (make-node 123 'abc NONE NONE))
(define node99new (make-node 99 'd NONE node123new))
(define node15new (make-node 15 'e node10new node24new))
(define node95new (make-node 95 'f NONE node99new))
(define node29new (make-node 29 'g node15new NONE))
(define node89new (make-node 89 'h node77new node95new))
(define node63new (make-node 63 'i node29new node89new))

(define node10newnew (make-node 10 'a NONE NONE))
(define node24newnew (make-node 24 'b NONE NONE))
(define node75newnew (make-node 75 'xyz NONE NONE))
(define node99newnew (make-node 99 'd NONE NONE))
(define node15newnew (make-node 15 'e node10newnew node24newnew))
(define node77newnew (make-node 77 'c node75newnew NONE))
(define node95newnew (make-node 95 'f NONE node99newnew))
(define node29newnew (make-node 29 'g node15newnew NONE))
(define node89newnew (make-node 89 'h node77newnew node95newnew))
(define node63newnew (make-node 63 'i node29newnew node89newnew))

(define treeA node63a)
(define treeAnew node63new)
(define treeAnewnew node63newnew)

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; BST Number Symbol -> BST
; Produce a BST that is just like B
; and that in place of one NONE subtree
; contains the node structure (make-node N S NONE NONE).
(define (create-bst B N S)
  (cond
    [(no-info? B)
     (make-node N S NONE NONE)]
    [else
     (cond
       [(= N (node-ssn B))
        (error ERR-MSG)]
       [(> N (node-ssn B))
        (make-node (node-ssn B)
                   (node-name B)
                   (node-left B)
                   (create-bst (node-right B) N S))]
       [(< N (node-ssn B))
        (make-node (node-ssn B)
                   (node-name B)
                   (create-bst (node-left B) N S)
                   (node-right B))])]))         

(check-error (create-bst treeA 99 'symbol) ERR-MSG)
(check-expect (create-bst NONE 123 'abc) (make-node 123 'abc NONE NONE))
(check-expect (create-bst treeA 123 'abc) treeAnew)
(check-expect (create-bst treeA 75 'xyz) treeAnewnew)
