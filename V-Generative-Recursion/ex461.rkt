;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex461) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define ε 0.01)
(define δ .1)
(define R 1000)

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; [Number -> Number] Number Number -> Number
; Approximate the area under the graph of f by dividing
; the interval [a, b] into sufficiently small intervals
; and then using the Kepler method.
; This algorithm is adaptive, i.e. it uses larger intervals
; for "level" parts of the graph and smaller intervals
; for "wavy" parts.
; Assumption: (<= a b)
(define (integrate-adaptive f a b)
  (local ((define error-margin (* ε (- b a)))
          (define whole-trapezoid (* 1/2 (- b a) (+ (f a) (f b))))
          (define trapezoid-halves (integrate-kepler f a b)))
    ; – IN –
    (cond
      [(<= (abs (- whole-trapezoid trapezoid-halves)) error-margin)
       (integrate-kepler f a b)]
      [else
       (local ((define mid (* 1/2 (+ a b))))
         ; – IN –
         (+ (integrate-adaptive f a mid)
            (integrate-adaptive f mid b)))])))
 
(check-within (integrate-adaptive (lambda (x) 20) 12 22) 200 δ)
(check-within (integrate-adaptive (lambda (x) (* 2 x)) 0 10) 100 δ)
(check-within (integrate-adaptive (lambda (x) (* 3 (sqr x))) 0 10) 1000 δ)

; from ex459.rkt ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; [Number -> Number] Number Number -> Number
; Compute the area under the graph of f between a and b.
; Assumption: (< a b) holds.
(define (integrate-rectangles f a b)
  (local ((define width (/ (- b a) R))

          ; Number -> Number
          ; Approximate the area under the graph of f
          ; using R rectangles.
          (define (approximate-area n)
            (cond
              [(>= n R) 0]
              [else
               (local ((define height (f (+ a (* n width) (/ width 2)))))
                 ; – IN –
                 (+ (* width height)
                    (approximate-area (add1 n))))])))
;               (+ (* width (f (+ a (* n width) (/ width 2))))
;                  (approximate-area (add1 n)))])))
    ; – IN –
    (approximate-area 0)))

; from ex458.rkt ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; [Number -> Number] Number Number -> Number
; Compute the area under the graph of f between a and b.
; Assumption: (< a b) holds.
(define (integrate-kepler f a b)
  (local ((define mid (/ (+ a b) 2))

          ; Number Number -> Number
          ; Calculate the are of the trapezoid
          ; between points x and y.
          ; Assume: (<= x y).
          (define (trapezoid-area x y)
            (* 1/2 (- y x) (+ (f y) (f x)))))
    ; – IN –
    (+ (trapezoid-area a mid)
       (trapezoid-area mid b))))

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(integrate-adaptive (lambda (x) 20) 12 22)
(integrate-rectangles (lambda (x) 20) 12 22)
(integrate-kepler (lambda (x) 20) 12 22)

(integrate-adaptive (lambda (x) (* 2 x)) 0 10)
(integrate-rectangles (lambda (x) (* 2 x)) 0 10)
(integrate-kepler (lambda (x) (* 2 x)) 0 10)

(integrate-adaptive (lambda (x) (* 3 (sqr x))) 0 10)
(integrate-rectangles (lambda (x) (* 3 (sqr x))) 0 10)
(integrate-kepler (lambda (x) (* 3 (sqr x))) 0 10)

(time (integrate-adaptive (lambda (x) (* 3 (sqr x))) 0 10000))
(time (integrate-rectangles (lambda (x) (* 3 (sqr x))) 0 10000))
(time (integrate-kepler (lambda (x) (* 3 (sqr x))) 0 10000))

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Does integrate-adaptive always compute a better answer than either
; integrate-kepler or integrate-rectangles?

; Generally, integrate-adaptive will give more precise answers than
; integrate-kepler, but not necessarily than integrate-rectangles.
; This is due to the way how it is determined that an answer is
; good enough. In integrate-kepler, there is no such exactness boundary, the
; interval is simply divided into two.
; Also, the exactness of integrate-adaptive depends on what function is used
; to do the actual integration.

; Which aspect is integrate-adaptive guaranteed to improve?

; For exetremely "wavy" functions it will spend more time on dividing the graph
; and should thus be more precise than the other functions.
