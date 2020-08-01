;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex153-interactive) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

(define-struct lob [n balloons])
; An LOB (list of balloons) is a structure:
;     (make-lob N List-of-Posn)
; Interpretation:
;     n gives the number of balloons to
;     be yet created and added to balloons.
;     balloons is a list with the
;     coordinates of the balloons
;     in the lecture hall.

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
(define (main n)
  (big-bang (make-lob n '())
    [on-draw render]
    [on-tick add-balloon 1]
    [stop-when end-of-balloons?]))

; LOB -> Image
; Return an image with n balloons in the
; lecture hall.
(define (render lob)
  (add-balloons (lob-balloons lob)))

(check-expect (render (make-lob 10 '()))
              LECTURE-HALL)
(check-expect (render (make-lob 10 (cons (make-posn 10 10) '())))
              (add-balloons (cons (make-posn 10 10) '())))

; NOTE - - - - - - - - - - - - - - - - -
; Cannot test functions calling random,
; check-random does not work here.
; END NOTE - - - - - - - - - - - - - - - 
; LOB -> LOB
; Add one balloon to the balloons-field
; and subtract 1 from the n-field.
(define (add-balloon lob)
  (make-lob (sub1 (lob-n lob))
            (cons (first (create-lop 1))
                  (lob-balloons lob))))

; LOB -> Boolean
; Are there no more balloons to be
; added to the balloons-field, i.e. is
; the number in the n-field negative?
(define (end-of-balloons? lob)
  (negative? (lob-n lob)))

(check-expect (end-of-balloons? (make-lob 10 '()))
              #false)
(check-expect (end-of-balloons? (make-lob 0 '()))
              #false)
(check-expect (end-of-balloons? (make-lob -1 '()))
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

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

; Figure 60 shows the output of our solution when given some list of Posns. 
; The left-most is the clean lecture hall, the second one is after two balloons 
; have hit, and the last one is a highly unlikely distribution of 10 hits. 
; Where is the 10th?

; One possible explanation is that the balloons started to hit in two waves,
; one starting from the top left corner and the other from the bottom right
; corner, moving to the middle of the lecture hall, each wave consisting of
; five balloons. Thus two ballons would have landed in the middle of the
; lecture hall.