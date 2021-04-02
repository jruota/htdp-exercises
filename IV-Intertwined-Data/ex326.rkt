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

; A BST (short for binary search tree) is
; a BT according to the following conditions:
; – NONE is always a BST.
; – (make-node ssn0 name0 L R) is a BST if
;   – L is a BST,
;   – R is a BST,
;   – all ssn fields in L are smaller than ssn0,
;   – all ssn fields in R are larger than ssn0.

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define NINE (make-node 99 'i NONE NONE))
(define EIGHT (make-node 24 'e NONE NONE))
(define SEVEN (make-node 87 'd NONE NONE))
(define SIX (make-node 95 'h NONE NINE))
(define FIVE (make-node 33 'g NONE NONE))
(define FOUR (make-node 15 'c SEVEN EIGHT))
(define THREE (make-node 89 'f FIVE SIX))
(define TWO (make-node 29 'b FOUR NONE))

(define TREE-B (make-node 63 'a TWO THREE))

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
         (if (contains-bt? B N)
             B
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
                 (create-bst (node-right B) N S))]))]))

(check-expect (create-bst NONE 63 'a)
              (make-node 63 ' a NONE NONE))
; there cannot be tow nodes with the same ssn
(check-expect (create-bst TREE-A 77 'abc)
              TREE-A)
(check-expect (create-bst TREE-A 83 'x)
              (make-node
               63 'a
               (make-node
                29 'b
                (make-node
                 15 'c
                 (make-node
                  10 'd NONE NONE)
                 (make-node
                  24 'e NONE NONE))
                NONE)
               (make-node
                89 'f
                (make-node
                 77 'g NONE
                 (make-node
                  83 'x NONE NONE))
                (make-node
                 95 'h NONE
                 (make-node 99 'i NONE NONE)))))
(check-expect (create-bst TREE-A 18 'abc)
              (make-node
               63 'a
               (make-node
                29 'b
                (make-node
                 15 'c
                 (make-node
                  10 'd NONE NONE)
                 (make-node
                  24 'e
                  (make-node
                   18 'abc NONE NONE)
                  NONE))
                NONE)
               (make-node
                89 'f
                (make-node
                 77 'g NONE NONE)
                (make-node
                 95 'h NONE
                 (make-node 99 'i NONE NONE)))))

; BT Number -> Boolean
; Does n occur in bt?
(define (contains-bt? bt n)
  (cond
    [(no-info? bt) #false]
    [else
     (or (= (node-ssn bt) n)
         (contains-bt? (node-left bt) n)
         (contains-bt? (node-right bt) n))]))

(check-expect (contains-bt? NONE 77)
              #false)
(check-expect (contains-bt? TREE-A 77)
              #true)
(check-expect (contains-bt? TREE-B 77)
              #false)
(check-expect (contains-bt? TREE-A 99)
              #true)
(check-expect (contains-bt? TREE-B 99)
              #true)
(check-expect (contains-bt? TREE-A 100)
              #false)
(check-expect (contains-bt? TREE-B 100)
              #false)
                         