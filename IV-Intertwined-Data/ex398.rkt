;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex398) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [List-of Number] [List-of Number] -> Number
; Calculate the value of the polynomial
; specified by lc for the values specified by vv.
(define (value lc vv)
  (cond
    [(empty? lc) 0]
    [else
     (+ (* (first lc) (first vv))
        (value (rest lc) (rest vv)))]))

(check-expect (value '() '())
              0)
(check-expect (value (list 5) (list 10))
              50)
(check-expect (value (list 5 17) (list 10 1))
              67)
(check-expect (value (list 5 17 3) (list 10 1 2))
              73)