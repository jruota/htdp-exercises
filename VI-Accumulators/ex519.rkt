;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex519) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Is it acceptable to impose the extra cost on cons for all programs to turn
; length into a constant-time function?

    ; While programs that regularly need the length of a given list would
    ; significantly profit from a constant time function (especially for very
    ; large lists), other programs that do not need the length of a list but
    ; need to construct a lot of lists would see a drop in permformance.
