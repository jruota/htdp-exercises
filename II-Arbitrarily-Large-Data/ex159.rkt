;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex159) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; An N is one of: 
; – 0
; – (add1 N)
; interpretation represents the counting numbers

; A List-of-Posn is one of:
; – '()
; – (cons Posn List-of-Posn)
; where the following constraints apply:
;     – 0 <= x <= WIDTH
;     – 0 <= y <= HEIGHT
; Interpretation:
;     A list of coordinates where
;     balloons have hit.

(define-struct pair [balloon# lob])
; A Pair is a structure (make-pair N List-of-posns)

; A List-of-posns is one of: 
; – '()
; – (cons Posn List-of-posns)
; interpretation (make-pair n lob) means n balloons 
; must yet be thrown and added to lob

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define DOT (circle 5 "solid" "orange"))
(define WIDTH 80)
(define HEIGHT 180)

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; to create some data

; N Image -> Image
; Produce a vertical arrangement of
; n copies of img.
(define (column n img)
  (cond
    [(zero? n) empty-image]
    [(positive? n)
     (overlay/xy img
                 0
                 (image-height img)
                 (column (sub1 n) img))]))

(check-expect (column 0 DOT)
              empty-image)
(check-expect (column 1 DOT)
              DOT)
(check-expect (column 2 DOT)
              (above DOT DOT))
(check-expect (column 5 DOT)
              (above DOT DOT DOT DOT DOT))

; N Image -> Image
; Produce a horizontal arrangement of
; n copies of img.
(define (row n img)
  (cond
    [(zero? n) empty-image]
    [(positive? n)
     (overlay/xy img
                 (image-width img)
                 0
                 (row (sub1 n) img))]))

(check-expect (row 0 DOT)
              empty-image)
(check-expect (row 1 DOT)
              DOT)
(check-expect (row 2 DOT)
              (beside DOT DOT))
(check-expect (row 5 DOT)
              (beside DOT DOT DOT DOT DOT))

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define BALLOON (circle 3 "solid" "red"))
(define LECTURE-HALL (place-image/align
                      (row 8
                           (column 18
                                   (rectangle 10 10 "outline" "black")))
                      0
                      0
                      "left"
                      "top"
                      (empty-scene WIDTH HEIGHT)))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; N -> Image
; Start the main program.
(define (riot n)
  (big-bang (make-pair n '())
    [on-draw render]
    [on-tick add-balloon 1]
    [stop-when end-of-balloons?]
    [check-with pair?]))

; Pair -> Image
; Return an image with n balloons in the
; lecture hall.
(define (render p)
  (add-balloons (pair-lob p)))

(check-expect (render (make-pair 10 '()))
              LECTURE-HALL)
(check-expect (render (make-pair 10 (cons (make-posn 10 10) '())))
              (add-balloons (cons (make-posn 10 10) '())))

; NOTE - - - - - - - - - - - - - - - - -
; Cannot test functions calling random,
; check-random does not work here.
; END NOTE - - - - - - - - - - - - - - - 
; Pair -> Pair
; Add one balloon to the balloons-field
; and subtract 1 from the n-field.
(define (add-balloon p)
  (make-pair (sub1 (pair-balloon# p))
             (cons (first (create-lop 1))
                   (pair-lob p))))

; Pair -> Boolean
; Are there no more balloons to be
; added to the lob-field, i.e. is
; the number in the balloon#-field negative?
(define (end-of-balloons? p)
  (negative? (pair-balloon# p)))

(check-expect (end-of-balloons? (make-pair 10 '()))
              #false)
(check-expect (end-of-balloons? (make-pair 0 '()))
              #false)
(check-expect (end-of-balloons? (make-pair -1 '()))
              #true)

; List-of-Posn -> Image
; Produce an image of the lecture hall
; with red dots added as specified by the Posns.
(define (add-balloons p)
  (cond
    [(empty? p) LECTURE-HALL]
    [(cons? p)
     (place-image BALLOON
                  (posn-x (first p))
                  (posn-y (first p))
                  (add-balloons (rest p)))]))

(check-expect (add-balloons '())
              LECTURE-HALL)
(check-expect (add-balloons (cons
                             (make-posn 40 90)
                             (cons
                              (make-posn 20 45)
                              (cons
                               (make-posn 60 135)
                               (cons
                                (make-posn 60 45)
                                (cons
                                 (make-posn 20 135)
                                 '()))))))
              (place-image
               BALLOON
               40
               90
               (place-image
                BALLOON
                20
                45
                (place-image
                 BALLOON
                 60
                 135
                 (place-image
                  BALLOON
                  60
                  45
                  (place-image
                   BALLOON
                   20
                   135
                   LECTURE-HALL))))))

; NOTE - - - - - - - - - - - - - - - - -
; Cannot test functions calling random,
; check-random does not work here.
; END NOTE - - - - - - - - - - - - - - - 
; N -> List-of-Posn
; Create a list of random Posns
; with n elements.
(define (create-lop n)
  (cond
    [(zero? n) '()]
    [(positive? n)
     (cons (make-posn (random WIDTH) (random HEIGHT))
           (create-lop (sub1 n)))]))