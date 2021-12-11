;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex494) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [List-of Number] -> [List-of Number]
; Produce a sorted version of l.
(define (sort> l)
  (cond
    [(empty? l) '()]
    [(cons? l)
     (insert (first l) (sort> (rest l)))]))

(check-expect (sort> '()) '())
(check-expect (sort> (list 3 2 1)) (list 3 2 1))
(check-expect (sort> (list 1 2 3)) (list 3 2 1))
(check-expect (sort> (list 12 20 -5))
              (list 20 12 -5))
 
; Number [List-of Number] -> [List-of Number]
; Insert n into the sorted list of numbers l. 
(define (insert n l)
  (cond
    [(empty? l) (cons n '())]
    [else (if (>= n (first l))
              (cons n l)
              (cons (first l) (insert n (rest l))))]))

(check-expect (insert 5 '()) (list 5))
(check-expect (insert 5 (list 6)) (list 6 5))
(check-expect (insert 5 (list 4)) (list 5 4))
(check-expect (insert 12 (list 20 -5)) (list 20 12 -5))

; [List-of Number] -> [List-of Number]
; Produce a sorted version of l.
(define (my-sort> l)
  (local (; [List-of Number] [List-of Number] -> [List-of Number]
          ; Produce a sorted version of l0.
          ; Collect intermediate solutions in accu.
          (define (my-sort-accu> l0 accu)
            (cond
              [(empty? l0) ...]
              [else
               ...])))
    ; – IN –
    ;(my-sort-accu> l '())))
    '()))

(check-expect (my-sort> '()) '())
(check-expect (my-sort> (list 3 2 1)) (list 3 2 1))
(check-expect (my-sort> (list 1 2 3)) (list 3 2 1))
(check-expect (my-sort> (list 12 20 -5))
              (list 20 12 -5))

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Does the insertion sort> function from Auxiliary Functions that Recur need an
; accumulator? If so, why? If not, why not?

; The first step from 32.1 Recognizing the Need for an Accumulator states

    ; If a structurally recursive function traverses the result of its natural
    ; recursion with an auxiliary, recursive function, consider the use of an
    ; accumulator parameter.

; This is the case here as sort> is a structurally recursive function and the
; auxiliary function insert traverses the result of its natural recursion.

; On the other hand, the accumulator has to use (first some-list) in some way,
; and in the case of sorting this would mean, that it would have to know the
; place of that value in the sorted list before the end of the list, which
; is not feasible. Therefore there is no use for an accumulator in sorting
; algorithms.

; The second step states

;     Our goal must be to understand whether the algorithm can fail to produce
;     a result for inputs for which we expect a result. If so, adding a
;     parameter that accumulates knowledge may help.

; But sorting a list of numbers never fails to deliver a result for
; a fixed-size data list of numbers. Adding another parameter, i.e. the
; accumulator, would therefore not help.

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;(define size 100)
;(define random-list (build-list size (lambda (x) (random size))))
;
;(time sort> random-list)
;(time my-sort> random-list)
