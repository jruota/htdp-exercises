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

; A BST (short for binary search tree) is
; a BT according to the following conditions:
; – NONE is always a BST.
; – (make-node ssn0 name0 L R) is a BST if
;   – L is a BST,
;   – R is a BST,
;   – all ssn fields in L are smaller than ssn0,
;   – all ssn fields in R are larger than ssn0.

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

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; [List-of [List Number Symbol]] -> BST
; Produce a BST from lolonas by repeatedly
; applying create-bst.
(define (create-bst-from-list lolonas)
  (cond
    [(empty? lolonas) NONE]
    [else
     (create-bst
      (create-bst-from-list (rest lolonas))
      (first (first lolonas))
      (second (first lolonas)))]))

(check-expect (create-bst-from-list '())
              NONE)
(check-expect (create-bst-from-list
               '((99 o)
                 (77 l)
                 (24 i)
                 (10 h)
                 (95 g)
                 (15 d)
                 (89 c)
                 (29 b)
                 (63 a)))
              TREE-A)

; [List-of [List Number Symbol]] -> BST
; Produce a BST from lolonas by repeatedly
; applying create-bst.
(define (create-bst-from-list.v2 lolonas)
  (cond
    [(empty? lolonas) NONE]
    [else
     (foldr
      (lambda (n s b) (create-bst b n s))
      NONE
      (map (lambda (x) (first x)) lolonas)
      (map (lambda (x) (second x)) lolonas))]))
     
(check-expect (create-bst-from-list.v2 '())
              NONE)
(check-expect (create-bst-from-list.v2
               '((99 o)
                 (77 l)
                 (24 i)
                 (10 h)
                 (95 g)
                 (15 d)
                 (89 c)
                 (29 b)
                 (63 a)))
              TREE-A)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

; If you use an existing abstraction, you may still get this tree but you may
; also get an “inverted” one. Why?

; If one where to use "foldl" instead of "foldr", the first element in the
; given list would be the root, not the last one.

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

; BST Number Symbol -> BST
; Produce a BST that is just like B
; and that in place of one NONE subtree
; contains the node structure
; (make-node N S NONE NONE).
(define (create-bst B N S)
  (cond
    [(no-info? B)
     (make-node N S NONE NONE)]
    [else
     (cond
       [(< N (node-ssn B))
        (make-node
         (node-ssn B)
         (node-name B)
         (create-bst (node-left B) N S)
         (node-right B))]
       [else
        (make-node
         (node-ssn B)
         (node-name B)
         (node-left B)
         (create-bst (node-right B) N S))])]))

(check-expect (create-bst NONE 63 'a)
              (make-node 63 ' a NONE NONE))
(check-expect (create-bst TREE-A 83 'x)
              (make-node
               63 'a
               (make-node
                29 'b
                (make-node
                 15 'd
                 (make-node
                  10 'h NONE NONE)
                 (make-node
                  24 'i NONE NONE))
                NONE)
               (make-node
                89 'c
                (make-node
                 77 'l NONE
                 (make-node
                  83 'x NONE NONE))
                (make-node
                 95 'g NONE
                 (make-node 99 'o NONE NONE)))))
                         