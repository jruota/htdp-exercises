;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex481) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define NUMBER-ERROR "n must be less than (+ QUEENS 1)")

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

(check-expect ((n-queens-solution? 4) (list (make-posn 0 1)
                                            (make-posn 1 3)
                                            (make-posn 2 0)
                                            (make-posn 3 2)))
              #true)
(check-expect ((n-queens-solution? 4) (list (make-posn 0 2)
                                            (make-posn 1 0)
                                            (make-posn 2 3)
                                            (make-posn 3 1)))
              #true)
(check-expect ((n-queens-solution? 5) (list (make-posn 0 0)
                                            (make-posn 1 2)
                                            (make-posn 2 4)
                                            (make-posn 3 1)
                                            (make-posn 4 3)))
              #true)

(check-expect ((n-queens-solution? 8) '()) #false)
(check-expect ((n-queens-solution? 4) (list (make-posn 0 2)
                                            (make-posn 1 0)
                                            (make-posn 2 3)
                                            (make-posn 3 3)))
              #false)
(check-expect ((n-queens-solution? 3) (list (make-posn 0 0)
                                            (make-posn 1 1)
                                            (make-posn 2 2)))
              #false)
(check-expect ((n-queens-solution? 6) (list (make-posn 2 2)))
              #false)

(check-error (n-queens-solution? 9) NUMBER-ERROR)

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

(check-expect (set=? '() '()) #true)
(check-expect (set=? '() (list 1 2 3 4 5)) #false)
(check-expect (set=? (list 1 2 3 4 5) '()) #false)
(check-expect (set=? (list 5 3 1 2 4 3 5) (list 1 2 5 2 4 2 3)) #true)
(check-expect (set=? (list 8 1 2 7 6 2 8) (list 8 1 6 2 7 9 1)) #false)

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

(check-expect (queens-threatening? (list (make-posn 0 1)
                                         (make-posn 1 3)
                                         (make-posn 2 0)
                                         (make-posn 3 2)))
              #false)
(check-expect (queens-threatening? (list (make-posn 0 0)
                                         (make-posn 1 2)
                                         (make-posn 2 4)
                                         (make-posn 3 1)
                                         (make-posn 4 3)))
              #false)

(check-expect (queens-threatening? (list (make-posn 0 7)
                                         (make-posn 1 2)
                                         (make-posn 2 5)
                                         (make-posn 3 0)
                                         (make-posn 4 4)
                                         (make-posn 5 1)    ; threatening
                                         (make-posn 6 6)
                                         (make-posn 7 3)))  ; threatening
              #true)

; [List-of X] -> [List-of X]
; Remove all duplicate items from lst.
(define (remove-duplicates lst)
  (cond
    [(empty? lst) '()]
    [else
     (if (member? (first lst) (rest lst))
         (remove-duplicates (rest lst))
         (cons (first lst) (remove-duplicates (rest lst))))]))

(check-expect (remove-duplicates '()) '())
(check-expect (remove-duplicates (list 1 2 7 3 99 2 5)) (list 1 7 3 99 2 5))
(check-expect (remove-duplicates (list 1 1 1 1 1)) (list 1))

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
