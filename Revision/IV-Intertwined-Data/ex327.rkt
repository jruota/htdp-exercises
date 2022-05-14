;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex327) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

(define node10a (make-node 10 'h NONE NONE))
(define node24a (make-node 24 'i NONE NONE))
(define node77a (make-node 77 'l NONE NONE))
(define node99a (make-node 99 'o NONE NONE))
(define node15a (make-node 15 'd node10a node24a))
(define node95a (make-node 95 'g NONE node99a))
(define node29a (make-node 29 'b node15a NONE))
(define node89a (make-node 89 'c node77a node95a))
(define node63a (make-node 63 'a node29a node89a))

(define treeA node63a)

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; [List-of [List Number Symbol]] -> BST
; Consume a list of numbers and names lonan and produce a BST. 
(define (create-bst-from-list lonan)
  (cond
    [(empty? lonan) NONE]
    [else
     (create-bst
      (create-bst-from-list (rest lonan))
      (first (first lonan))
      (second (first lonan)))]))

(check-expect (create-bst-from-list '()) NONE)
(check-expect (create-bst-from-list '((99 o)
                                      (77 l)
                                      (24 i)
                                      (10 h)
                                      (95 g)
                                      (15 d)
                                      (89 c)
                                      (29 b)
                                      (63 a)))
              treeA)

; [List-of [List Number Symbol]] -> BST
; Consume a list of numbers and names lonan and produce a BST. 
(define (create-bst-from-list.v2 lonan)
  (foldr
   (lambda (x y) (create-bst y (first x) (second x)))
   NONE
   lonan))

(check-expect (create-bst-from-list.v2 '()) NONE)
(check-expect (create-bst-from-list.v2 '((99 o)
                                         (77 l)
                                         (24 i)
                                         (10 h)
                                         (95 g)
                                         (15 d)
                                         (89 c)
                                         (29 b)
                                         (63 a)))
              treeA)

; from ex326.rkt ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
