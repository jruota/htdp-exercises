;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex290) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A [List-of ITEM] is one of:
;     – '()
;     – (cons ITEM [List-of ITEM])
; Interpretation:
;     A list of items.

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; NOTE +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

; This is not a true clone of the "append" function, since it can handle only
; two lists where "append" can handle an arbitrary amount of lists.

; END NOTE +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

; [X Y Z] [List-of X] [List-of Y] -> [List-of Z]
; Concatenate the items of lst1 and lst2.
(define (append-from-fold lst1 lst2)
    (foldr (lambda (x y)
             (cons x y))
           lst2 lst1))

(check-expect (append-from-fold '() '())
              '())
(check-expect (append-from-fold '() (list 1 2 3))
              (list 1 2 3))
(check-expect (append-from-fold (list 1 2 3) '())
              (list 1 2 3))
(check-expect (append-from-fold (list 1 2 3 4 5)
                                (list 6 7 8 9 10))
              (list 1 2 3 4 5 6 7 8 9 10))


; NOTE +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

; Using "foldl" instead of "foldr" reverses the order of the elements of lst1,
; but not of lst2.

; END NOTE +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

;; [X Y Z] [List-of X] [List-of Y] -> [List-of Z]
;; Concatenate the items of lst1 and lst2.
;(define (append-from-fold.v2 lst1 lst2)
;    (foldl (lambda (x y)
;             (cons x y))
;           lst2 lst1))
;
;(check-expect (append-from-fold.v2 '() '())
;              '())
;(check-expect (append-from-fold.v2 '() (list 1 2 3))
;              (list 1 2 3))
;(check-expect (append-from-fold.v2 (list 1 2 3) '())
;              (list 1 2 3))
;(check-expect (append-from-fold.v2 (list 1 2 3 4 5)
;                                   (list 6 7 8 9 10))
;              (list 1 2 3 4 5 6 7 8 9 10))

; [List-of Number] -> Number
; Calculate the sum of the
; numbers in lst.
(define (sum lst)
  (foldr (lambda (x y)
           (+ x y))
         0 lst))

(check-expect (sum '())
              0)
(check-expect (sum (list 1 2 3 4 5 6 7 8 9))
              45)

; [List-of Number] -> Number
; Calculate the product of the
; numbers in lst.
(define (product lst)
  (foldr (lambda (x y)
           (* x y))
         1 lst))

(check-expect (product '())
              1)
(check-expect (product (list 1 2 3 4 5 6 7 8 9))
              362880)

; [List-of Image] -> Image
; Horizontally compose the images in loi.
(define (beside-from-foldr loi)
  (foldr (lambda (x y)
           (beside x y))
         empty-image loi))

(check-expect (beside-from-foldr '())
              empty-image)
(check-expect (beside-from-foldr
               (list (square 100 "solid" "red")
                     (square 100 "solid" "orange")
                     (square 100 "solid" "yellow")))
              (beside (square 100 "solid" "red")
                      (square 100 "solid" "orange")
                      (square 100 "solid" "yellow")))

; [List-of Image] -> Image
; Horizontally compose the images in loi.
(define (beside-from-foldl loi)
  (foldl (lambda (x y)
           (beside x y))
         empty-image loi))

(check-expect (beside-from-foldl '())
              empty-image)
(check-expect (beside-from-foldl
               (list (square 100 "solid" "red")
                     (square 100 "solid" "orange")
                     (square 100 "solid" "yellow")))
              (beside (square 100 "solid" "yellow")
                      (square 100 "solid" "orange")
                      (square 100 "solid" "red")))

; [List-of Image] -> Image
; Horizontally compose the images in loi.
(define (above-from-foldr loi)
  (foldr (lambda (x y)
           (above x y))empty-image loi))

(check-expect (above-from-foldr '())
              empty-image)
(check-expect (above-from-foldr
               (list (square 100 "solid" "red")
                     (square 100 "solid" "orange")
                     (square 100 "solid" "yellow")))
              (above (square 100 "solid" "red")
                     (square 100 "solid" "orange")
                     (square 100 "solid" "yellow")))

; [List-of Image] -> Image
; Horizontally compose the images in loi.
(define (above-from-foldl loi)
  (foldl (lambda (x y)
           (above x y))
         empty-image loi))

(check-expect (above-from-foldl '())
              empty-image)
(check-expect (above-from-foldl
               (list (square 100 "solid" "red")
                     (square 100 "solid" "orange")
                     (square 100 "solid" "yellow")))
              (above (square 100 "solid" "yellow")
                     (square 100 "solid" "orange")
                     (square 100 "solid" "red")))