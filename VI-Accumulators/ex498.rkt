;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex498) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define-struct node [left right])
; A Tree is one of: 
; – '()
; – (make-node Tree Tree)

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define example
  (make-node (make-node '() (make-node '() '())) '()))

; FUNCTION DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Tree -> N
; Return the number of edges on the
; longest path on abt.
(define (height abt)
  (cond
    [(empty? abt) 0]
    [else (+ (max (height (node-left abt))
                  (height (node-right abt))) 1)]))

(check-expect (height '()) 0)
(check-expect (height example) 3)

; Tree -> N
; Return the number of edges on the
; longest path on abt0.
(define (height.v2 abt0)
  (local (; Tree N -> N
          ; Measure the height of abt.
          ; The accumulator a is the number of steps
          ; it takes to reach abt from abt0.
          (define (height/a abt a)
            (cond
              [(empty? abt) a]
              [else
                (max (height/a (node-left abt)
                               (+ a 1))
                     (height/a (node-right abt)
                               (+ a 1)))])))
    ; – IN –
    (height/a abt0 0)))

(check-expect (height.v2 '()) 0)
(check-expect (height.v2 example) 3)

; Tree -> N
; Return the number of edges on the
; longest path on abt0.
(define (height.v3 abt0)
  (local (; Tree N N -> N
          ; Measure the height of abt.
          ; Accumulator s is the number of steps
          ; it takes to reach abt from abt0.
          ; Accumulator m is the maximal height of
          ; the part of abt0 that is to the left of abt.
          (define (h/a abt s m)
            (cond
              [(empty? abt) (max s m)]
              [else
               (local ((define steps (+ s 1))
                       (define height-left
                         (h/a (node-left abt) steps steps)))
                 ; – IN –
                 (h/a (node-right abt) steps height-left))])))
    ; – IN –
    (h/a abt0 0 0)))

(check-expect (height.v3 '()) 0)
(check-expect (height.v3 example) 3)
