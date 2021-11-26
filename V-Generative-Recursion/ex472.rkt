;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex472) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

; Graph -> Boolean
; Is there a path between any pair of nodes in g?
(define (test-on-all-nodes g)
  (local ((define nodes (map (lambda (x) (first x)) g))

          ; [List-of Node] -> Boolean
          ; Helper function, does the actual work.
          (define (toan lon)
            (cond
              [(empty? lon) #true]
              [else
               (and (connects-to-all-nodes? (first lon)
                                            (remove (first lon) nodes))
                    (toan (rest lon)))]))

          ; Node [List-of Node] -> Boolean
          ; Are there paths from start to all nodes
          ; in end in g?
          (define (connects-to-all-nodes? start end)
            (cond
              [(empty? end) #true]
              [else
               (and (not (false? (find-path start (first end) g)))
                    (connects-to-all-nodes? start (rest end)))])))
    ; – IN –
    (toan nodes)))

(check-expect (test-on-all-nodes '())
              #true)
(check-expect (test-on-all-nodes sample-graph)
              #false)
(check-expect (test-on-all-nodes (list (list 'A 'B)
                                       (list 'B 'C)
                                       (list 'C 'D)
                                       (list 'D 'A)))
              #true)

; Node Node Graph -> [Maybe Path]
; finds a path from origination to destination in G
; if there is no path, the function produces #false
(define (find-path origination destination G)
  (cond
    [(symbol=? origination destination) (list destination)]
    [else (local ((define next (neighbors origination G))
                  (define candidate
                    (find-path/list next destination G)))
            (cond
              [(boolean? candidate) #false]
              [else (cons origination candidate)]))]))

(check-expect (find-path 'C 'D sample-graph)
              '(C D))
(check-member-of (find-path 'E 'D sample-graph)
                 '(E F D) '(E C D))
(check-expect (find-path 'C 'G sample-graph)
              #false)
 
; [List-of Node] Node Graph -> [Maybe Path]
; finds a path from some node on lo-Os to D
; if there is no path, the function produces #false
(define (find-path/list lo-Os D G)
  (cond
    [(empty? lo-Os) #false]
    [else (local ((define candidate
                    (find-path (first lo-Os) D G)))
            (cond
              [(boolean? candidate)
               (find-path/list (rest lo-Os) D G)]
              [else candidate]))]))

(check-expect (find-path/list '() 'A 'G)
              #false)
(check-member-of (find-path/list (list 'B 'E) 'G sample-graph)
                 (list 'B 'E 'F 'G)
                 (list 'B 'F 'G)
                 (list 'E 'F 'G))

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

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Use the function to find a path from 'A to 'G in sample-graph.
; Which one does it find? Why?

(find-path 'A 'G sample-graph)

; It finds (list 'A 'B 'E 'F 'G) and not (list 'A 'B 'F 'G) (or 'A 'E 'F 'G)
; because 'E is listed before 'F as a direct neighbor of 'B.
