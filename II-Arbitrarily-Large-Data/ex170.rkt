;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex170) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define-struct phone [area switch four])
; A Phone is a structure: 
;   (make-phone Three Three Four)
; A Three is a Number between 100 and 999. 
; A Four is a Number between 1000 and 9999.

; A List-of-Phones is one of:
; – '()
; – (cons Phone List-of-Phones)

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; List-of-Phones -> List-of-Phones
; Replace all occurrences of the area code 713
; with 281.
(define (replace lop)
  (cond
    [(empty? lop) '()]
    [(cons? lop)
     (if (area713? (first lop))
         (cons (change-area (first lop) 281)
               (replace (rest lop)))
         (cons (first lop)
               (replace (rest lop))))]))

(check-expect (replace '())
              '())
(check-expect (replace
               (cons (make-phone 713 808 9456)
                     (cons (make-phone 414 741 5215)
                           (cons (make-phone 713 354 3690)
                                 (cons (make-phone 606 008 5623) '())))))
              (cons (make-phone 281 808 9456)
                    (cons (make-phone 414 741 5215)
                          (cons (make-phone 281 354 3690)
                                (cons (make-phone 606 008 5623) '())))))

; Phone -> Phone
; Is p's area code equal to 713?
(define (area713? p)
  (= 713 (phone-area p)))

(check-expect (area713? (make-phone 713 808 9456))
              #true)
(check-expect (area713? (make-phone 414 741 5215))
              #false)

; Phone Three -> Phone
; Replace p's area code with a.
(define (change-area p a)
  (make-phone a (phone-switch p) (phone-four p)))

(check-expect (change-area (make-phone 414 741 5215) 000)
              (make-phone 000 741 5215))