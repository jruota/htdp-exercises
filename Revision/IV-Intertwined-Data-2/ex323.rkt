;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex323) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define-struct no-info [])
(define NONE (make-no-info))
 
(define-struct node [ssn name left right])
; A BT (short for BinaryTree) is one of:
; – NONE
; – (make-node Number Symbol BT BT)

; A ForS (false or symbol) is one of:
; – #false
; – Symbol

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

(define NINE-B (make-node 99 'i NONE NONE))
(define EIGHT-B (make-node 24 'e NONE NONE))
(define SEVEN-B (make-node 87 'd NONE NONE))
(define SIX-B (make-node 95 'h NONE NINE-B))
(define FIVE-B (make-node 33 'g NONE NONE))
(define FOUR-B (make-node 15 'c SEVEN-B EIGHT-B))
(define THREE-B (make-node 89 'f FIVE-B SIX-B))
(define TWO-B (make-node 29 'b FOUR-B NONE))

(define TREE-B (make-node 63 'a TWO-B THREE-B))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; BT N -> ForS
; If the tree bt contains a node structure
; whose ssn field is equal to n, produce the
; value of the name field in that node.
; Otherwise, return #false.
(define (search-bt bt n)
  (cond
    [(no-info? bt) #false]
    [else
     (if (= (node-ssn bt) n)
         (node-name bt)
         (local ((define left (search-bt (node-left bt) n))
                 (define right (search-bt (node-right bt) n)))
           ; – IN –
           (if (symbol? left) left right)))]))

(check-expect (search-bt NONE 123)
              #false)
(check-expect (search-bt TREE-A 100)
              #false)
(check-expect (search-bt TREE-A 99)
              'i)
(check-expect (search-bt TREE-B 23)
              #false)
(check-expect (search-bt TREE-B 24)
              'e)
