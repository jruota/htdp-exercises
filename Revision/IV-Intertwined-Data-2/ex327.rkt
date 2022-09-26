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
; – NONE is always a BST.
; – (make-node ssn0 name0 L R) is a BST if
; – L is a BST,
; – R is a BST,
; – all ssn fields in L are smaller than ssn0,
; – all ssn fields in R are larger than ssn0.

; A LoNN (list of numbers and names) is one of:
; – '()
; – (cons (list N Symbol) LoNN)

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define NINE-A (make-node 99 'o NONE NONE))
(define EIGHT-A (make-node 24 'i NONE NONE))
(define SEVEN-A (make-node 10 'h NONE NONE))
(define SIX-A (make-node 95 'g NONE NINE-A))
(define FIVE-A (make-node 77 'l NONE NONE))
(define FOUR-A (make-node 15 'd SEVEN-A EIGHT-A))
(define THREE-A (make-node 89 'c FIVE-A SIX-A))
(define TWO-A (make-node 29 'b FOUR-A NONE))

(define TREE-A (make-node 63 'a TWO-A THREE-A))

(define INPUT
  '((99 o) (77 l) (24 i) (10 h) (95 g) (15 d) (89 c) (29 b) (63 a)))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; LoNN -> BST
; Create a binary search tree from lonn.
(define (create-bst-from-list lonn)
  (cond
    [(empty? lonn) NONE]
    [else
     (local ((define n (first (first lonn)))
             (define s (second (first lonn))))
       ; – IN –
       (create-bst
        (create-bst-from-list (rest lonn))
        n
        s))]))

(check-expect (create-bst-from-list '())
              NONE)
(check-expect (create-bst-from-list INPUT)
              TREE-A)

; LoNN -> BST
; Create a binary search tree from lonn.
(define (create-bst-from-list.v2 lonn)
  (cond
    [(empty? lonn) NONE]
    [else
     (foldr
      (lambda (node acc)
        (create-bst acc (first node) (second node)))
      NONE
      lonn)]))

(check-expect (create-bst-from-list.v2 '())
              NONE)
(check-expect (create-bst-from-list.v2 INPUT)
              TREE-A)

; from ex326.rkt ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
