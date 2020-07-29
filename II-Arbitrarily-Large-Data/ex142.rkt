;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex142) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A List-of-Images is one of:
; – '()
; – (cons Image List-of-Images)
; Interpretation:
;     A list containing Images.

; ImageOrFalse is one of:
; – Image
; – #false

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define IMG1 (square 50 "solid" "orange"))
(define IMG2 (square 51 "solid" "red"))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; List-of-Images PositiveNumber -> Image / Boolean
; Return the first image that is not
; an n by n square. If there is no such
; image, return #false.
(define (ill-sized? loi n)
  (cond
    [(empty? loi) #false]
    [(cons? loi)
     (if (not (= (image-width (first loi))
                 (image-height (first loi))
                 n))
         (first loi)
         (ill-sized? (rest loi) n))]))

(check-expect (ill-sized? '() 50)
              #false)
(check-expect (ill-sized? (cons IMG1 '()) 50)
              #false)
(check-expect (ill-sized? (cons IMG2 '()) 50)
              IMG2)
(check-expect (ill-sized? (cons IMG1
                                (cons IMG1
                                      (cons IMG1
                                            (cons IMG1 '()))))
                          50)
              #false)
(check-expect (ill-sized? (cons IMG1
                                (cons IMG1
                                      (cons IMG1
                                            (cons IMG2 '()))))
                          50)
              IMG2)