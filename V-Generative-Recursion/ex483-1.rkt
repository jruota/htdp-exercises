;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex483-1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define QUEENS 8)
; A QP is a structure:
;   (make-posn CI CI)
; A CI is an N in [0,QUEENS).
; interpretation (make-posn r c) denotes the square at 
; the r-th row and c-th column

; An N is one of:
; – 0
; – (add1 N)
; Interpretation:
;     The natural numbers.

; A [Maybe X] is one of:
; – #false
; – X
; Interpretation:
;     #false or X.

; NOTE ---------------------------------------------------------------------
; By far the fastest data definition, becuase the board is a simple list
; that is created once and then gets significantly smaller for every step in
; the algorithm, since only the relevant information is kept, i.e. positions
; where a queen can still be placed. The function find-open-spots does not
; need to do anything.
; END NOTE -----------------------------------------------------------------
; A Board is a [List-of QP].
; Interpretation:
;     A Board collects those positions where a queen can still be placed.

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define NUMBER-ERROR "n must be less than (+ QUEENS 1) and greater than 0")

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; N -> [Maybe [List-of QP]]
; Find a solution to the n queens problem. 
(define (n-queens n)
  (place-queens (board0 n) n))

(check-satisfied (n-queens 4) (n-queens-solution? 4))

; Board N -> [Maybe [List-of QP]]
; Place n queens on a-board.
; Return #false otherwise.
(define (place-queens a-board n)
  (local (; [List-of QP] N -> [Maybe [List-of QP]]
          ; Return a list of QPs of non-threatening
          ; queens of length n0 using the possible
          ; positions in loqp.
          ; If there is no solution, return #false.
          (define (place-queens/list loqp n0)
            (cond
              [(zero? n)
               '()]
              [(and (empty? loqp) (> n 0))
               #false]
              [(and (cons? loqp) (> n 0))
               (local ((define candidate
                         (place-queens
                          (add-queen a-board (first loqp))
                          (sub1 n0))))
                 (cond
                   [(boolean? candidate)
                    (place-queens/list (rest loqp) n0)]
                   [else
                    (cons (first loqp) candidate)]))])))
    ; – IN –
    (place-queens/list (find-open-spots a-board) n)))

(check-expect (place-queens (board0 0) 0) '())
(check-expect (place-queens (board0 3) 3) #false)
(check-satisfied (place-queens (board0 4) 4) (n-queens-solution? 4))

; N -> Board 
; Create the initial n by n board.
(define (board0 n)
  (cond
    [(or (> n QUEENS) (< n 0))
     (error NUMBER-ERROR)]
    [else
     (local ((define half-pairs (build-list n (lambda (x) x)))

             ; [List-of X] -> [List-of (list X N)]
             ; Pair up every element of lox with
             ; the elements from half-pairs.
             (define (pair-up-list lox)
               (cond
                 [(empty? lox) '()]
                 [else
                  (append
                   (pair-up (first lox) half-pairs)
                   (pair-up-list (rest lox)))]))

             ; Y [List-of X] -> [List-of (list Y X)]
             ; Pair up m with the elements from lst.
             (define (pair-up m lst)
               (cond
                 [(empty? lst) '()]
                 [else
                  (cons
                   (make-posn m (first lst))
                   (pair-up m (rest lst)))])))
       ; – IN –
       (pair-up-list half-pairs))]))

(check-expect (board0 0) '())
(check-expect (board0 1) (list (make-posn 0 0)))
(check-expect (board0 2) (list (make-posn 0 0) (make-posn 0 1)
                               (make-posn 1 0) (make-posn 1 1)))
(check-expect
 (board0 4)
 (list (make-posn 0 0) (make-posn 0 1) (make-posn 0 2) (make-posn 0 3)
       (make-posn 1 0) (make-posn 1 1) (make-posn 1 2) (make-posn 1 3)
       (make-posn 2 0) (make-posn 2 1) (make-posn 2 2) (make-posn 2 3)
       (make-posn 3 0) (make-posn 3 1) (make-posn 3 2) (make-posn 3 3)))

(check-error (board0 (add1 QUEENS)) NUMBER-ERROR)
 
; Board QP -> Board 
; Place a queen at qp on a-board.
(define (add-queen a-board qp)
  (cond
    [(empty? a-board) '()]
    [else
     (local ((define first-chess-board-square (first a-board))
             (define rest-of-chess-board (add-queen (rest a-board) qp)))
       ; – IN –
       (if (threatening? qp first-chess-board-square)
           rest-of-chess-board
           (cons first-chess-board-square
                 rest-of-chess-board)))]))

(check-expect (add-queen (board0 0) (make-posn 0 0)) '())
(check-expect (add-queen (board0 2) (make-posn 1 1)) '())
(check-expect (add-queen (board0 4) (make-posn 2 2))
              (list (make-posn 0 1)
                    (make-posn 0 3)
                    (make-posn 1 0)
                    (make-posn 3 0)))
 
; Board -> [List-of QP]
; Find spots where it is still safe to place a queen.
(define (find-open-spots a-board)
  a-board)

(check-expect (find-open-spots '()) '())
; in the following test, the queen is placed at (2, 2)
(check-expect (find-open-spots (list (make-posn 0 1)
                                     (make-posn 0 3)
                                     (make-posn 1 0)
                                     (make-posn 3 0)))
              (list (make-posn 0 1)
                    (make-posn 0 3)
                    (make-posn 1 0)
                    (make-posn 3 0)))

; from ex481.rkt ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; N -> [[Maybe [List-of QP]] -> Boolean]
; Produce a predicate on queen placements that
; determines whether a given placement is a
; solution to an n queens puzzle.
(define (n-queens-solution? n)
  (local (; [Maybe [List-of QP]] -> Boolean
          ; Determine whether the queen placements
          ; given in mloqp are a solution to the
          ; n queens puzzle.
          (define (solves-n-queens-puzzle? mloqp)
            (cond
              [(false? mloqp) #false]
              [else
               (cond
                 [(not (= (length mloqp) n)) #false]
                 [else
                  (not (queens-threatening? mloqp))])])))
    ; – IN –
    (cond
      [(> n QUEENS)
       (error NUMBER-ERROR)]
      [else
       solves-n-queens-puzzle?])))

; [List-of X] [List-of Y] -> Boolean
; Determine whether s1 and s2 contain the same items,
; regardless of order.
(define (set=? s1 s2)
  (cond
    [(empty? s1) (empty? s2)]
    [else
     (local (; [List-of X] -> Boolean
             ; Are all of the elements of s0
             ; in set2?
             (define (all-elements-in-set2? s0)
               (cond
                 [(empty? s0) #true]
                 [else
                  (and (member? (first s0) set2)
                       (all-elements-in-set2? (rest s0)))]))
             
             (define set1 (remove-duplicates s1))
             (define set2 (remove-duplicates s2)))
       ; – IN –
       (if (= (length set1) (length set2))
           (all-elements-in-set2? set1)
           #false))]))

; [List-of QP] -> Boolean
; Do any of the queens at the positions specified
; by loqp threaten each other?
(define (queens-threatening? loqp)
  (local (; QP [List-of QP] -> Boolean
          ; Does qp threaten any of the queens
          ; in loqp0?
          (define (queen-threatening? qp loqp0)
            (cond
              [(empty? loqp0) #false]
              [else
               (or (threatening? qp (first loqp0))
                   (queen-threatening? qp (rest loqp0)))])))
    ; – IN –
    (cond
      [(empty? loqp) #false]
      [else
       (or (queen-threatening? (first loqp) (rest loqp))
           (queens-threatening? (rest loqp)))])))

; [List-of X] -> [List-of X]
; Remove all duplicate items from lst.
(define (remove-duplicates lst)
  (cond
    [(empty? lst) '()]
    [else
     (if (member? (first lst) (rest lst))
         (remove-duplicates (rest lst))
         (cons (first lst) (remove-duplicates (rest lst))))]))

; from ex479.rkt ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; QP QP -> Boolean
; Do the queens placed at one and two
; threaten each other?
(define (threatening? one two)
  (or
   ; horizontal
   (= (posn-x one) (posn-x two))
   ; vertical
   (= (posn-y one) (posn-y two))
   ; diagonal left top to right bottom
   (= (- (posn-x one) (posn-y one))
      (- (posn-x two) (posn-y two)))
   ; diagonal left bottom to right top
   (= (+ (posn-x one) (posn-y one))
      (+ (posn-x two) (posn-y two)))))

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(time (n-queens 8))