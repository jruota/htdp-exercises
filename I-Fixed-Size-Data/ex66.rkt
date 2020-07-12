;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex66) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct movie [title producer year])

; Fields
; – movie-title     -> String
; – movie-producer  -> String
; – movie-year      -> String

(make-movie "Woman at War" "Benedikt Erlingsson" 2018)

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define-struct person [name hair eyes phone])

; Fields
; – person-name     -> String
; – person-hair     -> String
; – person-eyes     -> String
; – person-phone    -> String

(make-person "John Doe" "brown" "grey" "555-123-4567")
    
; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
(define-struct pet [name number])

; Fields
; – pet-name        -> String
; – pet-number      -> Number

(make-pet "Bolt" 12345)
    
; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
(define-struct CD [artist title price])

; Fields
; – CD-artist       -> String
; – CD-title        -> String
; – CD-price        -> Number

(make-CD "Pearl Jam" "Lightning Bolt" 14.99)
    
; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
(define-struct sweater [material size producer])

; Fields
; – sweater-material -> String
; – sweater-size     -> String
; – sweater-producer -> String

(make-sweater "Cotton" "M" "Fruit of the Loom")