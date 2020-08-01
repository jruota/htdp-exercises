;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex154) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define-struct layer [color doll])

; An RD (short for Russian doll) is one of: 
; – String 
; – (make-layer String RD)

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; RD -> String
; Produce a string of all colors,
; separated by a comma and a space.
(define (colors rd)
  (cond
    [(string? rd) rd]
    [(layer? rd)
     (string-append (layer-color rd)
                    ", "
                    (colors (layer-doll rd)))]))

(check-expect (colors "red")
              "red")
(check-expect (colors (make-layer "yellow"
                                  (make-layer "green" "red")))
              "yellow, green, red")