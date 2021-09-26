;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex471) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A Node is a Symbol.

; A Graph is a [List-of [List-of Node]].
; Interpretation:
;     The nodes and their direct neighbors in a graph.

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define sample-graph-original
  '((A B E)
    (B E F)
    (C D)
    (D)
    (E C F)
    (F D G)
    (G)))

(define sample-graph
  (list (list 'A 'B 'E)
        (list 'B 'E 'F)
        (list 'C 'D)
        (list 'D)
        (list 'E 'C 'F)
        (list 'F 'D 'G)
        (list 'G)))

(check-expect sample-graph-original sample-graph)

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Node Graph -> [List-of Node]
; Return the list of immediate neighbours of n in g.
(define (neighbors n g)
  (cond
    [(empty? g) '()]
    [else
     (if (symbol=? (first (first g)) n)
         (rest (first g))
         (neighbors n (rest g)))]))

(check-expect (neighbors 'F sample-graph)
              (list 'D 'G))
(check-expect (neighbors 'G sample-graph)
              '())
(check-expect (neighbors 'H sample-graph)
              '())