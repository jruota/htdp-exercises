;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex256) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [X] [X -> Number] [NEList-of X] -> X 
; finds the (first) item in lx that maximizes f
; if (argmax f (list x-1 ... x-n)) == x-i, 
; then (>= (f x-i) (f x-1)), (>= (f x-i) (f x-2)), ...
; (define (argmax f lx) ...)

; EXPLANATION
; The return value of "argmax" is the value for wich f reaches a maximum. If
; there multiple such values, "argmax" returns the first one of these.

(argmax sin (list 0
                  (* 1/4 pi)
                  (* 2/4 pi)
                  (* 3/4 pi)
                  pi
                  (* 5/4 pi)
                  (* 6/4 pi)
                  (* 7/4 pi)
                  (* 2 pi)))

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; [X] [X -> Number] [NEList-of X] -> X 
; finds the (first) item in lx that minimizes f
; if (argmax f (list x-1 ... x-n)) == x-i, 
; then (<= (f x-i) (f x-1)), (<= (f x-i) (f x-2)), ...
; (define (argmin f lx) ...)

; EXPLANATION
; The return value of "argmin" is the value for wich f reaches a minimum. If
; there multiple such values, "argmin" returns the first one of these.

(argmin sin (list 0
                  (* 1/4 pi)
                  (* 2/4 pi)
                  (* 3/4 pi)
                  pi
                  (* 5/4 pi)
                  (* 6/4 pi)
                  (* 7/4 pi)
                  (* 2 pi)))