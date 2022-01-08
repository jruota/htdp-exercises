;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex520) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; The solve* function generates all states reachable with n boat trips before it
; looks at states that require n + 1 boat trips, even if some of those boat
; trips return to previously encountered states. Because of this systematic way
; of traversing the tree, solve* cannot go into an infinite loop. Why?
; Terminology:
;     This way of searching a tree or a graph is dubbed breadth-first search.

    ; The functions does not pursue a single sequence of steps and only checks
    ; others if this one does not lead to a solution (if in this case it would
    ; not use an accumulator to track previously encountered steps / states this
    ; could lead to an infinite loop and would make it impossible to test other
    ; sequences), but isntead generates the next steps for all previous possible
    ; states at the same time. Even if some sequences loop forever, others will
    ; eventually lead to a solution because the game is designed in such a way
    ; that it has at least one solution (found using a hand evaluation).
    ; So, if the game has a solution, the function will eventually find it.
