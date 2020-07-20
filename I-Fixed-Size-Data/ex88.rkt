;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex88) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct vcat [x hap])
; A VCat is a structure:
;     (make-vcat Number Number)
; where the following condition applies:
;     – hap is a number greater or equal to 0.
; Interpretation:
;     The cat's current x-coordinate
;     and its happiness level.