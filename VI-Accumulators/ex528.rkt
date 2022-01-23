;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex528) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define THRESHOLD 10)

(define WIDTH 450)
(define HEIGHT (* 16/9 WIDTH))
(define TEST-SCENE (empty-scene WIDTH HEIGHT))
(define COLOR "red")

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Image Posn Posn Posn -> Image
; Smooth the curve between points a and c
; with b as the perspective point and draw
; it in img.
(define (bezier img a b c)
  (local ((define xa (posn-x a))
          (define ya (posn-y a))
          (define xb (posn-x b))
          (define yb (posn-y b))
          (define xc (posn-x c))
          (define yc (posn-y c)))
    ; – IN –
    (cond
      [(small-enough? a b c)
       (scene+line
        (scene+line img xb yb xc yc COLOR)
        xa ya xb yb COLOR)]
      [else
       (local ((define mid-ab (mid-point a b))
               (define mid-bc (mid-point b c))
               (define mid-abc (mid-point mid-ab mid-bc))

               (define scene1 (bezier img a mid-ab mid-abc)))
         ; – IN –
         (bezier scene1 mid-abc mid-bc c))])))

; Posn Posn Posn -> Boolean
; Are all of the sides of the triangle
; formed by points a, b and c shorter
; than THRESHOLD.
(define (small-enough? a b c)
  (local ((define xa (posn-x a))
          (define ya (posn-y a))
          (define xb (posn-x b))
          (define yb (posn-y b))
          (define xc (posn-x c))
          (define yc (posn-y c))

          (define dist-ab (sqrt (+ (sqr (- xa xb)) (sqr (- ya yb)))))
          (define dist-bc (sqrt (+ (sqr (- xb xc)) (sqr (- yb yc)))))
          (define dist-ca (sqrt (+ (sqr (- xc xa)) (sqr (- yc ya))))))
    ; – IN –
    (and (< dist-ab THRESHOLD)
         (< dist-bc THRESHOLD)
         (< dist-ca THRESHOLD))))

(check-expect (small-enough? (make-posn 0 0)
                             (make-posn THRESHOLD 0)
                             (make-posn (* 1/2 THRESHOLD) (- THRESHOLD 1)))
              #false)
(check-expect (small-enough? (make-posn 0 0)
                             (make-posn (- THRESHOLD 1) 0)
                             (make-posn (* 1/2 (- THRESHOLD 1))
                                        (* (/ (sqrt 3) 2) (- THRESHOLD 1))))
              #true)

; from ex525.rkt ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Posn Posn -> Posn 
; determines the midpoint between a and b
(define (mid-point a b)
  (local ((define x1 (posn-x a))
          (define y1 (posn-y a))
          (define x2 (posn-x b))
          (define y2 (posn-y b)))
    ; – IN –
    (make-posn (* 1/2 (+ x1 x2))
               (* 1/2 (+ y1 y2)))))

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(bezier TEST-SCENE
        (make-posn 10 10)
        (make-posn (* 1/3 WIDTH) (* 1.6 HEIGHT))
        (make-posn (- WIDTH 10) (* 1/3 HEIGHT)))
