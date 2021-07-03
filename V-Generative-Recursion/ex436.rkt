;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex436) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define MAX 101)

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Posn -> Posn 
; Create a random position for the food.
; Termination:
;     Since a new position is created randomly, there is no way to predict
;     when a termination will be reached. It will not be reached if MAX is
;     is too small, i.e. is equal to 1 or 0.
(define (food-create p)
  (local (; Posn Posn -> Posn
          ; generative recursion
          ; Create a random position for the food
          ; that is different from the previous
          ; position p.
          (define (food-check-create p candidate)
            (if (equal? p candidate) (food-create p) candidate)))
    ; – IN –
    (food-check-create
     p (make-posn (random MAX) (random MAX)))))

(check-satisfied (food-create (make-posn 1 1)) not=-1-1?)
 
; Posn -> Boolean
; use for testing only 
(define (not=-1-1? p)
  (not (and (= (posn-x p) 1) (= (posn-y p) 1))))

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; The basic problem is creating a new, random position and comparing it to the
; existing one. If it is different, it is the solution to the problem. If it is
; not different, hand the problem over to another instance of "food-create".