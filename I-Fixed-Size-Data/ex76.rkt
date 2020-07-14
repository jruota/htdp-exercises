;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex76) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct movie [title producer year])
; A Movie is a structure:
;     (make-movie String String Number)
; Combines the title of the movie with
; its producer and year of production.

(define-struct person [name hair eyes phone])
; A Person is a structure:
;     (make-person String String String String)
; Combines the name of a person with its hair and
; eye color as well as his / her phone number.

(define-struct pet [name number])
; A Pet is a structure:
;     (make-pet String Number)
; Combines the pet's name with its
; identification number.

(define-struct CD [artist title price])
; A CD is a structure:
;     (make-CD String String Number)
; Combines the name of the artis with
; the CD title as well as its price.

(define-struct sweater [material size producer])
; A Sweater is a structure:
;     (make-sweater String String String)
; Combines the material of the sweater with its
; size and the producer of the sweater.