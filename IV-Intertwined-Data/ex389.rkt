;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex389) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define-struct phone-record [name number])
; A PhoneRecord is a structure:
;   (make-phone-record String String)

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define PHONE
  (list "123-456-7890"
        "555-935-2905"
        "890-104-1836"
        "987-123-9867"))

(define NAME
  (list "Janice"
        "Amanda"
        "William"
        "Philip"))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; [List-of String] [List-of String] -> [List-of PhoneRecord]
; Create a list of phone records from names and numbers.
(define (zip names numbers)
  (cond
    [(empty? names) '()]
    [else
     (cons
      (make-phone-record (first names) (first numbers))
      (zip (rest names) (rest numbers)))]))

(check-expect (zip '() '())
              '())
(check-expect (zip NAME PHONE)
              (list
               (make-phone-record "Janice" "123-456-7890")
               (make-phone-record "Amanda" "555-935-2905")
               (make-phone-record "William" "890-104-1836")
               (make-phone-record "Philip" "987-123-9867")))