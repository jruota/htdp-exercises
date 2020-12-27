;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex280) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; (+ 1 (* 1 2))
; ==
; (+ 1 2)
; ==
; 3
((lambda (x y) (+ x (* x y)))
 1 2)

; ––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––

; (+ 1 (+ (* 3 (* 2 2)) (/ 1 1)))
; ==
; (+ 1 (+ (* 3 4) 1))
; ==
; (+ 1 (+ 12 1))
; ==
; (+ 1 13)
; ==
; 14
((lambda (x y)
   (+ x
      (local ((define z (* y y)))
        (+ (* 3 z) (/ 1 x)))))
 1 2)

; ––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––

; (+ 1 ((lambda ...) (* 2 2)))
; ==
; (+ 1 ((lambda ...) 4))
; ==
; (+ 1 (+ (* 3 4) (/ 1 4)))
; ==
; (+ 1 (+ 12 1/4))
; ==
; (+ 1 49/4)
; ==
; 53/4
; ==
; 13.25
((lambda (x y)
   (+ x
      ((lambda (z)
         (+ (* 3 z) (/ 1 z)))
       (* y y))))
 1 2)