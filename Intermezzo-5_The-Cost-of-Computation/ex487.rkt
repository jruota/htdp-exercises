;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex487) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; N -> N
; Map numbers to numbers
; according to the definition
; of the function.
(define (f n)
  (expt 2 n))

; N -> N
; Map numbers to numbers
; according to the definition
; of the function.
(define (g n)
  (* 1000 n))

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;                                   f(n)                                    g(n)

;   1                                  2                                    1000

;   3                                  8                                    3000

;  10                               1024                                   10000

;  12                               4096                                   12000

;  13                               8192                                   13000

;  14                              16384                                   14000

; 100    1267650600228229401496703205376                                  100000

; Therefore f(n) >= g(n) for all n >= 14.
; That is c = 1 and bigEnough = 14.
