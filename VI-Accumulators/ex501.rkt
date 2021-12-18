;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex501) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; N -> Number 
; Add n to pi without using +.
(define (add-to-pi n)
  (cond
    [(zero? n) pi]
    [else (add1 (add-to-pi (sub1 n)))]))

(check-within (add-to-pi 0) pi 0.001)
(check-within (add-to-pi 2) (+ 2 pi) 0.001)

; N -> Number 
; Add n to pi without using +.
(define (add-to-pi.v2 n)
  (local (; N Number -> Number
          ; Add n to pi without using +.
          ; The accumulator accu is the intermidiary result
          ; of adding up to (* i 1) to pi where
          ; 0 <= i <= n.
          (define (add-to-pi/a n0 accu)
            (cond
              [(zero? n0) accu]
              [else
               (add-to-pi/a (sub1 n0) (add1 accu))])))
    ; – IN –
    (add-to-pi/a n pi)))

(check-within (add-to-pi.v2 0) pi 0.001)
(check-within (add-to-pi.v2 2) (+ 2 pi) 0.001)

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define size 100000)

(time (add-to-pi size))
(time (add-to-pi.v2 size))
