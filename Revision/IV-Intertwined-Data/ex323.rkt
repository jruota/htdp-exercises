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

; Number BT -> MaybeSymbol
; If the tree contains a node structure whose ssn field is n,
; produce the value of the name field in that node.
; Otherwise, produce #false.
(define (search-bt n bt)
  (cond
    [(no-info? bt) #false]
    [else
     (cond
       [(= (node-ssn bt) n)
        (node-name bt)]
       [(contains-bt? n (node-left bt))
        (search-bt n (node-left bt))]
       [(contains-bt? n (node-right bt))
        (search-bt n (node-right bt))]
       [else
        #false])]))

(check-expect (search-bt 23 NONE) #false)
(check-expect (search-bt 77 treeA) 'c)
(check-expect (search-bt 77 treeB) #false)

; Number BT -> MaybeSymbol
; If the tree contains a node structure whose ssn field is n,
; produce the value of the name field in that node.
; Otherwise, produce #false.
(define (search-bt.v2 n bt)
  (local (; BT -> Symbol
          ; Return the name field of the node in bt0
          ; whose ssn field is equal to n.
          (define (main bt0)
            (cond
              [(no-info? bt0)
               #false]
              [(= (node-ssn bt0) n)
               (node-name bt0)]
              [else
               (local ((define left (main (node-left bt0)))
                       (define right (main (node-right bt0))))
                 ; – IN –
                 (if (boolean? left) right left))])))
    ; – IN –
    (cond
      [(contains-bt? n bt)
       (main bt)]
      [else
       #false])))

(check-expect (search-bt.v2 23 NONE) #false)
(check-expect (search-bt.v2 77 treeA) 'c)
(check-expect (search-bt.v2 77 treeB) #false)

; from ex322.rkt ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Number BT -> Boolean
; Is n in bt?
(define (contains-bt? n bt)
  (cond
    [(no-info? bt) #false]
    [else
     (or (= (node-ssn bt) n)
         (contains-bt? n (node-left bt))
         (contains-bt? n (node-right bt)))]))
