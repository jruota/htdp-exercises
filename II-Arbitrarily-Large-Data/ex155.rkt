;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex155) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define-struct layer [color doll])

; An RD (short for Russian doll) is one of: 
; – String 
; – (make-layer String RD)

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; RD -> String
; Produce (the color of) the
; innermost doll.
(define (inner rd)
  (cond
    [(string? rd) rd]
    [(layer? rd)
     (inner (layer-doll rd))]))

(check-expect (inner "red")
              "red")
(check-expect (inner (make-layer "yellow"
                                 (make-layer "green" "red")))
              "red")