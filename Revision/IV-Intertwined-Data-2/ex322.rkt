;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex322) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

(make-node
  15
  'd
  NONE
  (make-node
    24 'i NONE NONE))

;      15
;      |___
;          |
;          24

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

(make-node
  15
  'd
  (make-node
    87 'h NONE NONE)
  NONE)

;      15
;   ___|
;  |
; 87

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

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

; BT N -> Boolean
; Does the binary tree bt contain
; a node with number n in its ssn field?
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
