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
; – #false
; – Symbol
; Interpretation:
;     A Symbol or #false.

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

; BT Number -> MaybeSymbol
; Return the value of the name-field
; if bt contains n in the ssn-field.
; If there is no such node, return #false.
(define (search-bt bt n)
  (cond
    [(contains-bt? bt n)
     ; – LOCAL STARTS HERE –
     (local ((define LEFT (search-bt (node-left bt) n))
             (define RIGHT (search-bt (node-right bt) n)))
       ; – IN –
       (cond
         [(= (node-ssn bt) n)
          (node-name bt)]
         [(symbol? LEFT) LEFT]
         [(symbol? RIGHT) RIGHT]))]
     ; – LOCAL ENDS HERE –
    [else
     #false]))

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

; BT Number -> MaybeSymbol
; Return the value of the name-field
; if bt contains n in the ssn-field.
; If there is no such node, return #false.
(define (search-bt.v2 bt n)
  (cond
    [(no-info? bt) #false]
    [else
     (cond
       [(= (node-ssn bt) n)
        (node-name bt)]
       [else
        (local ((define LEFT (search-bt.v2 (node-left bt) n))
                (define RIGHT (search-bt.v2 (node-right bt) n)))
          ; – IN –
          (if (boolean? LEFT)
              RIGHT
              LEFT))])]))

(check-expect (search-bt.v2 NONE 123)
              #false)
(check-expect (search-bt.v2 TREE-A 100)
              #false)
(check-expect (search-bt.v2 TREE-A 99)
              'i)
(check-expect (search-bt.v2 TREE-B 23)
              #false)
(check-expect (search-bt.v2 TREE-B 24)
              'e)

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