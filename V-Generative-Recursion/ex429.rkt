;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex429) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [List-of Number] Number -> [List-of Number]
; Return all numbers from lon that are less
; than n.
(define (smallers lon n)
  (filter (lambda (x) (< x n)) lon))

(check-expect (smallers (list 11 8 14 7) 11)
              (list 8 7))
(check-expect (smallers (list 11 9 2 18 12 14 4 1) 11)
              (list 9 2 4 1))

; [List-of Number] Number -> [List-of Number]
; Return all numbers from lon that are greater
; than n.
(define (largers lon n)
  (filter (lambda (x) (> x n)) lon))

(check-expect (largers (list 11 8 14 7) 11)
              (list 14))
(check-expect (largers (list 11 9 2 18 12 14 4 1) 11)
              (list 18 12 14))