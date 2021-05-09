;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex364) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; An Xexpr is a list: 
; – (cons Symbol Body)
; – (cons Symbol (cons [List-of Attribute] Body))
; where Body is short for [List-of Xexpr]

; An Attribute is a list of two items:
;   (cons Symbol (cons String '()))

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;<transition from="seen-e" to="seen-f"></transition>
'(transition ((from "seen-e") (to "seen-f")))

;<ul>
;  <li>
;    <word></word>
;    <word></word>
;  </li>
;  <li>
;    <word></word>
;  </li>
;</ul>
(list 'ul
      (list 'li
            (list 'word 'word))
      (list 'li
            (list 'word)))

'(ul
  (li
   (word word))
  (li (word)))