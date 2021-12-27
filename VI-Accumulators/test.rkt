;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname test) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

(define test (explode "abcdefghijklmnopqrstuvwxyz"))
(define font-size 20)
(define font-color "orange")

(text (implode test) font-size font-color)

; [List-of 1String] N -> String
; Return a (partial) string s from lo1s
; whose image representation has a width
; that is less than or equal than x and
; where s and another 1String from lo1s
; have a width greater than or equal than x.
(define (test-func lo1s x)
  (cond
    [(or (empty? lo1s) (<= x 0)) ""]
    [(and (cons? lo1s) (> x 0))
     (local ((define letter (first lo1s))
             (define str-width
               (image-width (text letter font-size font-color))))
       ; – IN –
       (cond
         [(<= str-width x)
          (string-append letter
                         (test-func (rest lo1s) (- x str-width)))]
         [else
          ""]))]))
