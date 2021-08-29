;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex459) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define ε 0.1)
(define R 100)

;(define ε 0.01)
;(define R 1000)

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
 
(check-within (integrate-rectangles (lambda (x) 20) 12 22) 200 ε)
(check-within (integrate-rectangles (lambda (x) (* 2 x)) 0 10) 100 ε)
(check-within (integrate-rectangles (lambda (x) (* 3 (sqr x))) 0 10) 1000 ε)