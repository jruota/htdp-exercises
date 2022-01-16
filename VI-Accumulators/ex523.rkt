;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex523) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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
; A LongPuzzleState is a structure:
;     (make-long-puzzle People People RightOrLeft [List-of PuzzleState])
; Interpretation:
;     The number of people on the left bank and the right bank of the river
;     as well as the number of people in the boat.
;     The last field lops records all states preceding the current one.

; A [Pair-of N] is a list:
;     (list N N)
; Interpretation:
;     A list of two natural numbers.

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

(define initial-11 (make-puzzle (make-people 2 3)
                                (make-people 1 0)
                                "right"
                                (list initial)))
(define initial-12 (make-puzzle (make-people 1 3)
                                (make-people 2 0)
                                "right"
                                (list initial)))
(define initial-13 (make-puzzle (make-people 2 2)
                                (make-people 1 1)
                                "right"
                                (list initial)))

(define initial-23 (make-puzzle (make-people 2 3)
                                (make-people 1 0)
                                "left"
                                (list initial initial-13)))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; [List-of PuzzleState] -> [List-of PuzzleState]
; Generate the list of all those states
; that a boat ride can reach for every
; PuzzleState in lops.
(define (create-next-states lops)
  (local ((define list-of-pairs (list (list 0 1) (list 0 2) (list 1 1)
                                      (list 2 0) (list 1 0)))

          ; [List-of PuzzleState] -> [List-of PuzzleState]
          ; Generate the list of all those states
          ; that a boat ride can reach for every
          ; PuzzleState in lops.
          (define (main lops0)
            (cond
              [(empty? lops0) '()]
              [else
               (if (final? (first lops0))
                   (main (rest lops0))
                   (append (next-states (first lops0))
                           (main (rest lops0))))]))

          ; PuzzleState -> [List-of PuzzleState]
          ; Generate the list of all those states
          ; that a boat ride can reach starting
          ; from ps.
          (define (next-states ps)
            (local ((define cannibals-left (people-cannibals (puzzle-left ps)))
                    (define missionaries-left
                      (people-missionaries (puzzle-left ps)))
                    (define cannibals-right
                      (people-cannibals (puzzle-right ps)))
                    (define missionaries-right
                      (people-missionaries (puzzle-right ps)))
                    (define boat (puzzle-boat ps))
                    (define states-so-far (puzzle-lops ps))

                    ; [List-of [Pair-of N]] -> [List-of PuzzleState]
                    ; Given lopon, which represents the passengers on
                    ; boat trips, return a list of all valid states
                    ; resulting from these trips.
                    (define (generate-next-states lopon)
                      (cond
                        [(empty? lopon) '()]
                        [else
                         (local ((define candidate
                                   (generate-next-state (first lopon))))
                           ; – IN –
                           (if (boolean? candidate)     ; candidate is #false
                               (generate-next-states (rest lopon))
                               (cons candidate
                                     (generate-next-states (rest lopon)))))]))

                    ; [Pair-of N] -> [Maybe PuzzleState]
                    ; Given pon, which represents the passengers
                    ; on a boat trip, return the state resulting
                    ; from this boat trip.
                    ; If this state is not valid, i.e. there are more
                    ; cannibals than missionaries on one side of the
                    ; river, return #false.
                    (define (generate-next-state pon)
                      (local ((define cannibal-on-boat (first pon))
                              (define missionary-on-boat (second pon)))
                        ; – IN –
                        (cond
                          [(string=? boat "left")
                           (validate (make-puzzle
                                      (make-people
                                       (- cannibals-left cannibal-on-boat)
                                       (- missionaries-left missionary-on-boat))
                                      (make-people
                                       (+ cannibals-right cannibal-on-boat)
                                       (+ missionaries-right
                                          missionary-on-boat))
                                      "right"
                                      (append (puzzle-lops ps) (list ps))))]
                          [(string=? boat "right")
                           (validate (make-puzzle
                                      (make-people
                                       (+ cannibals-left cannibal-on-boat)
                                       (+ missionaries-left missionary-on-boat))
                                      (make-people
                                       (- cannibals-right cannibal-on-boat)
                                       (- missionaries-right
                                          missionary-on-boat))
                                      "left"
                                      (append (puzzle-lops ps) (list ps))))])))

                    ; PuzzleState -> [Maybe PuzzleState]
                    ; If ps0 is a valid PuzzleState and has not
                    ; been encountered on the way to ps, return ps0.
                    ; Otherwise return #false.
                    (define (validate ps0)
                      (local ((define cl (people-cannibals (puzzle-left ps0)))
                              (define ml
                                (people-missionaries (puzzle-left ps0)))
                              (define cr (people-cannibals (puzzle-right ps0)))
                              (define mr
                                (people-missionaries (puzzle-right ps0)))
                              (define b (puzzle-boat ps0))

                              ; [List-of PuzzleState] -> Boolean
                              ; Is ps0 the same as any of the states
                              ; in lops1?
                              (define (compare-states lops1)
                                (cond
                                  [(empty? lops1) #false]
                                  [else
                                   (or (compare-state (first lops1))
                                       (compare-states (rest lops1)))]))

                              ; PuzzleState -> Boolean
                              ; Is ps0 the same as ps1?
                              ; Ignores the lops field of both ps0 and ps1.
                              (define (compare-state ps1)
                                (local
                                  ((define people-left1 (puzzle-left ps1))
                                   (define people-right1 (puzzle-right ps1))

                                   (define cl1 (people-cannibals people-left1))
                                   (define ml1
                                     (people-missionaries people-left1))
                                   (define cr1
                                     (people-cannibals people-right1))
                                   (define mr1
                                     (people-missionaries people-right1))
                                   (define b1 (puzzle-boat ps1)))
                             ; – IN –
                             (and (= cl cl1) (= ml ml1)
                                  (= cr cr1) (= mr mr1)
                                  (string=? b b1)))))
                        ; – IN –
                        (cond
                          [(and (>= cl 0) (>= ml 0) (>= cr 0) (>= mr 0)
                                (or (>= ml cl) (= ml 0))
                                (or (>= mr cr) (= mr 0))
                                (not (compare-states states-so-far)))
                           ps0]
                          [else #false]))))
              ; – IN –
              (generate-next-states list-of-pairs))))
    ; – IN –
    (main lops)))

(check-expect (create-next-states '()) '())
(check-expect (create-next-states (list final))
              '())
(check-member-of (create-next-states (list initial))
                 (list initial-11 initial-12 initial-13)
                 (list initial-11 initial-13 initial-12)
                 (list initial-12 initial-11 initial-13)
                 (list initial-12 initial-13 initial-11)
                 (list initial-13 initial-11 initial-12)
                 (list initial-13 initial-12 initial-11))
(check-member-of (create-next-states (list final initial))
                 (list initial-11 initial-12 initial-13)
                 (list initial-11 initial-13 initial-12)
                 (list initial-12 initial-11 initial-13)
                 (list initial-12 initial-13 initial-11)
                 (list initial-13 initial-11 initial-12)
                 (list initial-13 initial-12 initial-11))
(check-member-of (create-next-states (list initial final))
                 (list initial-11 initial-12 initial-13)
                 (list initial-11 initial-13 initial-12)
                 (list initial-12 initial-11 initial-13)
                 (list initial-12 initial-13 initial-11)
                 (list initial-13 initial-11 initial-12)
                 (list initial-13 initial-12 initial-11))
(check-expect (create-next-states (list initial-13))
              (list initial-23))

; from ex521.rkt ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
