;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex346) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; NOTE *************************************************************************

; The examples up to this exercise suggest that the data definition should
; include numbers only. But exercise 348 includes Booleans and figure 43 in
; Intermezzo 1 includes numbers, booleans, strings and images.
; Then again, between exercises 348 and 349 it says:
;    "For example, #true, "hello", and '(+ x 1) are not 
;     representatives of BSL expressions."

; END NOTE *********************************************************************

; A BSL-val is a Number.

; A BSL-val is one of:
; – Number
; – Boolean
; – String
; – Image
