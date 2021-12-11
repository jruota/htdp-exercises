;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex492) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A Node is a Symbol.

; A Graph is a [List-of [List-of Node]].
; Interpretation:
;     The nodes and their direct neighbors in a graph.

; A Path is a [List-of Node].
; Interpretation:
;     The list of nodes specifies a sequence
;     of immediate neighbors that leads from the first 
;     Node on the list to the last one.

; A [Maybe X] is one of: 
; – #false 
; – X

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define sample-graph
  '((A B E)
    (B E F)
    (C D)
    (D)
    (E C F)
    (F D G)
    (G)))

(define directed-graph-with-cycle
  (list (list 'A 'B 'E)
        (list 'B 'E 'F)
        (list 'C 'B 'D)
        (list 'D)
        (list 'E 'C 'F)
        (list 'F 'D 'G)
        (list 'G)))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Node Node Graph -> [Maybe Path]
; Find a path from origination to destination in G.
; If there is no path, produce #false.
(define (find-path origination destination G)
  (find-path-accu origination destination G '()))

(check-expect (find-path 'C 'D sample-graph)
              '(C D))
(check-member-of (find-path 'E 'D sample-graph)
                 '(E F D) '(E C D))
(check-expect (find-path 'C 'G sample-graph)
              #false)

(check-member-of (find-path 'B 'D directed-graph-with-cycle)
                 '(B F D) '(B E F D) '(B E C D))
  
; Node Node Graph [List-of Node] -> [Maybe Path]
; Find a path from origin to destination in G.
; If there is no path, produce #false.
; Assume there are no paths from the nodes in seen.
(define (find-path-accu origin destination G seen)
  (cond
    [(symbol=? origin destination) (list destination)]
    [(member? origin seen) #false]
    [else
     (local ((define next (neighbors origin G))
             (define candidate
               (find-path/list next
                               destination
                               G
                               (cons origin seen))))
       ; – IN –
       (cond
         [(boolean? candidate) #false]
         [else (cons origin candidate)]))]))

(check-expect (find-path-accu 'A 'A directed-graph-with-cycle '())
              (list 'A))
(check-expect (find-path-accu 'A 'D directed-graph-with-cycle (list 'A))
              #false)
(check-member-of (find-path-accu 'A 'D directed-graph-with-cycle '())
                 '(A B F D) '(A B E C D) '(A B E F D)
                 '(A E C D) '(A E F D))
 
; [List-of Node] Node Graph [List-of Node] -> [Maybe Path]
; Find a path from some node on lo-Os to D.
; If there is no path, produce #false.
; Assume there are no paths from the nodes in seen.
(define (find-path/list lo-Os D G seen)
  (cond
    [(empty? lo-Os) #false]
    [else (local ((define first-node (first lo-Os))
                  (define candidate (find-path-accu first-node D G seen)))
            ; – IN –
            (cond
              [(boolean? candidate)
               (find-path/list (rest lo-Os) D G (cons first-node seen))]
              [else candidate]))]))

(check-expect (find-path/list '() 'G sample-graph '())
              #false)
(check-member-of (find-path/list (list 'B 'E) 'G sample-graph '())
                 (list 'B 'E 'F 'G)
                 (list 'B 'F 'G)
                 (list 'E 'F 'G))

(check-member-of (find-path/list (list 'C) 'G directed-graph-with-cycle '())
                 '(C B F G) '(C B E F G))

; from ex471.rkt ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
