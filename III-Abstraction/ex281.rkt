;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex281) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; ––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––

(lambda (x) (< x 10))

((lambda (x) (< x 10)) 9)
((lambda (x) (< x 10)) 10)
((lambda (x) (< x 10)) 11)

; ––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––

(lambda (x y) (number->string (* x y)))
((lambda (x y) (number->string (* x y))) 2 3)

; ––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––

(lambda (x)
  (cond
    [(even? x) 0]
    [(odd? x) 1]))

((lambda (x)
  (cond
    [(even? x) 0]
    [(odd? x) 1])) 2)

((lambda (x)
  (cond
    [(even? x) 0]
    [(odd? x) 1])) 5)

; ––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––

(define-struct IR [name price])
; An IR is a structure:
;   (make-IR String Number)

(lambda (x y) (< (IR-price x) (IR-price y)))
((lambda (x y) (< (IR-price x) (IR-price y))) (make-IR "car" 25000)
                                              (make-IR "pen" 1.99))

; ––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––

(define SCENE (empty-scene 200 200))
(define DOT (circle 3 "solid" "red"))

(lambda (p img) (place-image DOT
                             (posn-x p)
                             (posn-y p)
                             img))

((lambda (p img) (place-image DOT
                             (posn-x p)
                             (posn-y p)
                             img))
 (make-posn 100 100)
 SCENE)