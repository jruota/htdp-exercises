;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex246) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A [List-of-ITEM] is one of:
; – '()
; – (cons ITEM [List-of-ITEM])
; Interpretation:
;     A list of items.

; FUNCTION DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Primitive List-of-NUMBERS Number -> List-of-NUMBERS
; Return a list with all numbers in l that are in a
; certail relation to the threshold t specified
; by the primitive R.
(define (extract R l t)
  (cond
    [(empty? l) '()]
    [else (cond
            [(R (first l) t)
             (cons (first l)
                   (extract R (rest l) t))]
            [else
             (extract R (rest l) t)])]))

(check-expect (extract < (list 1 2 3 4 5 6 7 8 9) 5)
              (list 1 2 3 4))

(check-expect (extract > (list 1 2 3 4 5 6 7 8 9) 5)
              (list 6 7 8 9))

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Step through the first eleven steps.
(extract < (cons 6 (cons 4 '())) 5)