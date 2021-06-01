;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex387) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; An OP (short for ordered pair) is a list
; of length two where the first element is
; a symbol and the second a number.

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; [List-of Symbol] [List-of Number] -> [List-of OP]
; Produce all possible ordered pairs of symbols and numbers.
(define (cross l1 l2)
  (local (; Symbol [List-of Number] -> [List-of OP]
          ; Pair s with every element in lon.
          (define (make-pairs s lon)
            (cond
              [(empty? lon) '()]
              [else
               (map (lambda (x) (list s x)) lon)])))
    ; – IN –
    (cond
      [(empty? l1) '()]
      [else
       (append
        (make-pairs (first l1) l2)
        (cross (rest l1) l2))])))

(check-expect (cross '(a b c) '(1 2))
              '((a 1) (a 2) (b 1) (b 2) (c 1) (c 2)))
(check-expect (cross '(a b c) '())
              '())
(check-expect (cross '() '(1 2))
              '())
(check-expect (cross '() '())
              '())