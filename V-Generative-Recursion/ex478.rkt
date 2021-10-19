;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex478) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; You can also place the first queen in all squares of the top-most row, the
; right-most column, and the bottom-most row. Explain why all of these solutions
; are just like the three scenarios depicted in figure 173.
;     Whether the first queen is placed in the top-most row, the right-most
;     column, or the bottom-most row, one can always rotate the chess board such
;     that the queen ends up in the left column. Thus one ends up with the same
;     problem.

; This leaves the central square. Is it possible to place even a second queen
; after you place one on the central square of a 3 by 3 board?
;     No, since the queen would threaten all other squares on the chess board.
;     See Figure 172.
