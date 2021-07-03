;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex437) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [List-of X] -> [List-of X]
; A general, structurally recursive function.
(define (special P)
  (cond
    [(empty? P) (solve P)]
    [else
     (combine-solutions
       P
       (special (rest P)))]))

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; special computes the length of its input

;(define (solve x)
;  0)
;
;(define (combine-solutions x y)
;  (+ 1 y))
;
;(check-expect (special (range 1 11 1)) 10)

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; special negates each number on the given list of numbers

;(define (solve x)
;  '())
;
;(define (combine-solutions x y)
;  (cons (if (> (first x) 0)
;            (* -1 (first x))
;            (first x))
;        y))
;
;(check-expect (special (range 5 -6 -1)) (list -5 -4 -3 -2 -1 0 -1 -2 -3 -4 -5))

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; special uppercases the given list of strings

(define (solve x)
  '())

(define (combine-solutions x y)
  (cons (if (string=? "" (first x))
            ""
            (string-append (string-upcase (substring (first x) 0 1))
                           (substring (first x) 1 (string-length (first x)))))
        y))

(check-expect (special (list "hello" "" "world" "banana" "orange" "alabama"))
              (list "Hello" "" "World" "Banana" "Orange" "Alabama"))
