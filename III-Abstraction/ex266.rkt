;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex266) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
((local ((define (f x) (+ x 3))
         (define (g x) (* x 4)))
   (if (odd? (f (g 1)))
       f
       g))
 2)

; (define (f_0 x) (+ x 3)
; (define (g_0 x) (* x 4)
;
; ((if (odd? (f_0 (g_0 1)))
;      f_0
;      g_0)
;  2)
;
; ==
; (define (f_0 x) (+ x 3)
; (define (g_0 x) (* x 4)
;
; ((if (odd? ((lambda (x) (+ x 3)) (g_0 1 )))
;      f_0
;      g_0)
;  2)
; ==
; (define (f_0 x) (+ x 3)
; (define (g_0 x) (* x 4)
;
; ((if (odd? ((lambda (x) (+ x 3))
;             ((lambda (x) (* x 4)) 1)))
;      f_0
;      g_0)
;  2)
; ==
; (define (f_0 x) (+ x 3)
; (define (g_0 x) (* x 4)
;
; ((if (odd? ((lambda (x) (+ x 3))
;             ((lambda (x) (* 1 4)))
;      f_0
;      g_0)
;  2)
; ==
; (define (f_0 x) (+ x 3)
; (define (g_0 x) (* x 4)
;
; ((if (odd? ((lambda (x) (+ x 3))
;             ((lambda (x) (* 1 4)))
;      f_0
;      g_0)
;  2)
; ==
; (define (f_0 x) (+ x 3)
; (define (g_0 x) (* x 4)
;
; ((if (odd? ((lambda (x) (+ x 3)) 4))
;      f_0
;      g_0)
;  2)
; ==
; (define (f_0 x) (+ x 3)
; (define (g_0 x) (* x 4)
;
; ((if (odd? ((lambda (x) (+ x 3)) 4))
;      f_0
;      g_0)
;  2)
; ==
; (define (f_0 x) (+ x 3)
; (define (g_0 x) (* x 4)
;
; ((if (odd? (+ 4 3))
;      f_0
;      g_0)
;  2)
; ==
; (define (f_0 x) (+ x 3)
; (define (g_0 x) (* x 4)
;
; ((if (odd? (+ 4 3))
;      f_0
;      g_0)
; ==
; (define (f_0 x) (+ x 3)
; (define (g_0 x) (* x 4)
;
; ((if (odd? 7)
;      f_0
;      g_0)
;  2)
; ==
; (define (f_0 x) (+ x 3)
; (define (g_0 x) (* x 4)
;
; ((if #true
;      f_0
;      g_0)
;  2)
; ==
; (f_0 2)
; ==
; (lambda (x) (+ x 3)) 2)
; ==
; (+ 2 3)
; ==
; 5