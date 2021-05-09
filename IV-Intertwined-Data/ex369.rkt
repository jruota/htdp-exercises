;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex369) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; An Xexpr is a list: 
; – (cons Symbol Body)
; – (cons Symbol (cons [List-of Attribute] Body))
; where Body is short for [List-of Xexpr]

; An Attribute is a list of two items:
;   (cons Symbol (cons String '()))

; An SorF is one of:
; – String
; – #false

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define LOA (list
             (list 'drama "drama")
             (list 'sci-fi "sci-fi")
             (list 'romance "romance")
             (list 'action "action")
             (list 'comedy "comedy")))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; [List-of Attribute] Symbol -> SorF
; If loa associates s with a string,
; return the string. Otherwise return #false.
(define (find-attr loa s)
  (local ((define RES (assq s loa)))
    (if (boolean? RES)
        #false
        (second RES))))

(check-expect (find-attr '() 'action)
              #false)
(check-expect (find-attr LOA 'history)
              #false)
(check-expect (find-attr LOA 'action)
              "action")