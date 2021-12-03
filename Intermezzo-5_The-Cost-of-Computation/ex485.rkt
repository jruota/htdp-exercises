;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex485) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; An NT (number tree) is one of:
; – Number
; – (list NT NT)

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define nt0 (list 1 2))
(define nt1 (list 3 4))
(define nt2 (list 5 6))
(define nt3 (list 7 8))

(define nt4 (list nt0 nt1))
(define nt5 (list nt2 nt3))

(define nt6 (list nt4 nt5))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; NT -> Number
; Determine the sum of the numbers in a tree.
(define (sum-tree nt)
  (cond
    [(number? nt) nt]
    [else
     (+ (sum-tree (first nt))
        (sum-tree (second nt)))]))

(check-expect (sum-tree 5) 5)
(check-expect (sum-tree nt6) (+ 1 2 3 4 5 6 7 8))

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; What is its abstract running time?
;     In the best case, there is only one function call to determine the result.
;     In the worst case, every function call sets of another two function calls.
;     So there are 2n function calls in the worst case, where n is the number
;     of nodes in the tree.
;     Therefore, sum-tree uses on the order of n steps
;     where n is the number of nodes in the tree.

; What is an acceptable measure of the size of such a tree?
;     The number of edges or nodes in the tree.

; What is the worst possible shape of the tree?
;     Except for the very last number trees that contain two numbers, the tree
;     would contain only other number trees.

; What’s the best possible shape?
;     A number.
