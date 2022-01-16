;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex522) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A RightOrLeft is one of:
; – "right"
; – "left"

(define-struct people [cannibals missionaries])
; A People is a structure:
;     (make-people N N)
; Interpretation:
;     The number of cannibals and missionaries.

(define-struct puzzle [left right boat lops])
; A PuzzleState is a structure:
;     (makepuzzle People People RightOrLeft [List-of PuzzleState])
; Interpretation:
;     The number of people on the left bank and the right bank of the river
;     as well as the number of people in the boat.
;     The last field lops records all states preceding the current one.

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; NOTE -------------------------------------------------------------------------
; The final and intermediate states have empty lops-fields. This would not occur
; during the solving process of a puzzle, but conforms to the data definition.
; The functions final? and render-mc do not need to be modified for this new
; data definition, since no change has been made to the previously present
; fields.
; END NOTE ---------------------------------------------------------------------

(define initial (make-puzzle (make-people 3 3)
                             (make-people 0 0)
                             "left"
                             '()))
(define final (make-puzzle (make-people 0 0)
                           (make-people 3 3)
                           "right"
                           '()))
(define intermediate (make-puzzle (make-people 1 1)
                                  (make-people 2 2)
                                  "left"
                                  '()))

(define RADIUS 20)
(define TRANSPARENT-SQUARE (square (* 4 RADIUS) "solid" (color 255 255 255 0)))
(define CANNIBAL (overlay (circle RADIUS "solid" "red")
                          TRANSPARENT-SQUARE))
(define MISSIONARY (overlay
                    (circle RADIUS "solid" "orange")
                    (square (* 4 RADIUS) "solid" (color 255 255 255 0))))
(define BOAT (above (rhombus (* 7/5 RADIUS) 90 "solid" "royalblue")
                    (rectangle (* 11/5 RADIUS) RADIUS "solid" "royalblue")))
(define WAVE (text " ~ " (* 4 RADIUS) "deepskyblue"))
(define WAVES (beside WAVE WAVE WAVE WAVE))
(define SHORT-WAVES (beside WAVE WAVE))
(define RIVER (above WAVES SHORT-WAVES WAVES))
(define HIEGHT (image-height RIVER))

(define RIVER-WIDTH (image-width RIVER))
(define RIVER-HEIGHT (image-height RIVER))
(define BOAT-WIDTH (image-width BOAT))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; PuzzleState -> Boolean
; Detect whether all people in ps (puzzle state)
; are on the right river bank as well as the boat.
(define (final? ps)
  (local ((define cannibals-left (people-cannibals (puzzle-left ps)))
          (define missionaries-left (people-missionaries (puzzle-left ps)))
          (define cannibals-right (people-cannibals (puzzle-right ps)))
          (define missionaries-right (people-missionaries (puzzle-right ps)))
          (define boat (puzzle-boat ps)))
    ; – IN –
    (and (zero? cannibals-left)
         (zero? missionaries-left)
         (= cannibals-right 3)
         (= missionaries-right 3)
         (string=? boat "right"))))

(check-expect (final? initial) #false)
(check-expect (final? final) #true)
(check-expect (final? intermediate) #false)

; PuzzleState -> Image
; Return the image representation of ps.
(define (render-mc ps)
  (local ((define cannibals-left (people-cannibals (puzzle-left ps)))
          (define missionaries-left (people-missionaries (puzzle-left ps)))
          (define cannibals-right (people-cannibals (puzzle-right ps)))
          (define missionaries-right (people-missionaries (puzzle-right ps)))
          (define boat (puzzle-boat ps))

          ; RightOrLeft -> Image
          ; Render the boat on the left or right
          ; river bank depending on rol.
          (define (render-boat-on-river rol)
            (place-image BOAT
                         (if (string=? rol "left")
                             (+ BOAT-WIDTH 2)
                             (- RIVER-WIDTH (+ BOAT-WIDTH 2)))
                         (+ (* RIVER-HEIGHT 1/2) -3)
                         RIVER))

          ; N 1String -> Image
          ; Render n representations of p, where p
          ; can be either "m" for missionary or "c"
          ; for cannibal.
          ; If n is zero, return TRANSPARENT-SQUARE.
          (define (render-people n p)
            (local ((define PEOPLE
                      (cond
                        [(string=? p "m") MISSIONARY]
                        [(string=? p "c") CANNIBAL]))
                    
                    ; N -> Image
                    ; Like render-people, but return
                    ; an empty-image if n0 is zero.
                    (define (rp n0)
                      (cond
                        [(zero? n0) empty-image]
                        [else
                         (above PEOPLE
                                (rp (sub1 n0)))])))
              ; – IN –
              (cond
                [(zero? n) TRANSPARENT-SQUARE]
                [else (rp n)]))))
    ; – IN –
    (beside (render-people missionaries-left "m")
            (render-people cannibals-left "c")
            (render-boat-on-river boat)
            (render-people missionaries-right "m")
            (render-people cannibals-right "c"))))

(check-expect (render-mc initial)
              (beside/align "middle"
                            (above MISSIONARY MISSIONARY MISSIONARY)
                            (above CANNIBAL CANNIBAL CANNIBAL)
                            (place-image BOAT
                                         (+ (image-width BOAT) 2)
                                         (+ (* (image-height RIVER) 1/2) -3)
                                         RIVER)
                            TRANSPARENT-SQUARE
                            TRANSPARENT-SQUARE))
(check-expect (render-mc final)
              (beside/align "middle"
                            TRANSPARENT-SQUARE
                            TRANSPARENT-SQUARE
                            (place-image BOAT
                                         (- (image-width RIVER)
                                            (+ (image-width BOAT) 2))
                                         (+ (* (image-height RIVER) 1/2) -3)
                                         RIVER)
                            (above MISSIONARY MISSIONARY MISSIONARY)
                            (above CANNIBAL CANNIBAL CANNIBAL)))
(check-expect (render-mc intermediate)
              (beside/align "middle"
                            MISSIONARY
                            CANNIBAL
                            (place-image BOAT
                                         (+ (image-width BOAT) 2)
                                         (+ (* (image-height RIVER) 1/2) -3)
                                         RIVER)
                            (above MISSIONARY MISSIONARY)
                            (above CANNIBAL CANNIBAL)))
