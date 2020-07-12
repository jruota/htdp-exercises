;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex70) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct centry [name home office cell])
; A ContactListEntry is a Structure:
;     (make-centry String Phone Phone Phone)
; Interpretation:
;     Combines the name of a contact and its
;     phone numbers for home, the office and
;     a mobile phone.

(define-struct phone [area number])
; A Phone is a Structure:
;     (make-phone Number String)
; Interpretation:
;     Combines the area code and the local number
;     of a phone number.

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(phone-area
 (centry-office
  (make-centry "Shriram Fisler"
    (make-phone 207 "363-2421")
    (make-phone 101 "776-1099")
    (make-phone 208 "112-9981")))) 
