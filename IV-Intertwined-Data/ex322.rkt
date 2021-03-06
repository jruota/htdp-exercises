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

(define TREE-A
  (make-node
   63
   'a
   (make-node
    29
    'b
    (make-node
     15
     'c
     (make-node
      10
      'd
      NONE
      NONE)
     (make-node
      24
      'e
      NONE
      NONE))
    NONE)
   (make-node
    89
    'f
    (make-node
     77
     'g
     NONE
     NONE)
    (make-node
     95
     'h
     NONE
     (make-node
      99
      'i
      NONE
      NONE)))))

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