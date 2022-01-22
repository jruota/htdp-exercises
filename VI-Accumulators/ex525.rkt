;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex525) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define THRESHOLD 10)

(define WIDTH 800)
(define HEIGHT (* 9/16 WIDTH))
(define TEST-SCENE (empty-scene WIDTH HEIGHT))
(define COLOR "red")

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Image Posn Posn Posn -> Image 
; generative adds the triangle (a, b, c) to scene0, 
; subdivides it into three triangles by taking the 
; midpoints of its sides; stop if (a, b, c) is too small
; accumulator the function accumulates the triangles of scene0
(define (add-sierpinski scene0 a b c)
  (cond
    [(too-small? a b c) scene0]
    [else
     (local
       ((define scene1 (add-triangle scene0 a b c))
        (define mid-a-b (mid-point a b))
        (define mid-b-c (mid-point b c))
        (define mid-c-a (mid-point c a))
        (define scene2
          (add-sierpinski scene1 a mid-a-b mid-c-a))
        (define scene3
          (add-sierpinski scene2 b mid-b-c mid-a-b)))
       ; —IN—
       (add-sierpinski scene3 c mid-c-a mid-b-c))]))

; Image Posn Posn Posn -> Image 
; adds the black triangle a, b, c to scene
(define (add-triangle scene a b c)
  (local ((define x1 (posn-x a))
          (define y1 (posn-y a))
          (define x2 (posn-x b))
          (define y2 (posn-y b))
          (define x3 (posn-x c))
          (define y3 (posn-y c)))
    ; – IN –
    (scene+line
     (scene+line
      (scene+line scene x1 y1 x2 y2 COLOR)
      x2 y2 x3 y3 COLOR)
     x3 y3 x1 y1 COLOR)))

(check-expect (add-triangle TEST-SCENE
                            (make-posn 10 10)
                            (make-posn (- WIDTH 10) (* 1/2 HEIGHT))
                            (make-posn 10 (- HEIGHT 10)))
              (scene+line
               (scene+line
                (scene+line TEST-SCENE 10 10 (- WIDTH 10) (* 1/2 HEIGHT) COLOR)
                (- WIDTH 10) (* 1/2 HEIGHT) 10 (- HEIGHT 10) COLOR)
               10 (- HEIGHT 10) 10 10 COLOR))
 
; Posn Posn Posn -> Boolean 
; is the triangle a, b, c too small to be divided
(define (too-small? a b c)
  (local ((define x1 (posn-x a))
          (define y1 (posn-y a))
          (define x2 (posn-x b))
          (define y2 (posn-y b))
          (define x3 (posn-x c))
          (define y3 (posn-y c))

          (define dist12 (sqrt (+ (sqr (- x1 x2)) (sqr (- y1 y2)))))
          (define dist23 (sqrt (+ (sqr (- x2 x3)) (sqr (- y2 y3)))))
          (define dist31 (sqrt (+ (sqr (- x3 x1)) (sqr (- y3 y1))))))
    ; – IN –
    (or (< dist12 THRESHOLD)
        (< dist23 THRESHOLD)
        (< dist31 THRESHOLD))))

(check-expect (too-small? (make-posn 0 0)
                          (make-posn 0 THRESHOLD)
                          (make-posn THRESHOLD THRESHOLD))
              #false)
(check-expect (too-small? (make-posn 0 0)
                          (make-posn (* 1/2 (- THRESHOLD 1)) THRESHOLD)
                          (make-posn (- THRESHOLD 1) 0))
              #true)
 
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

(check-expect (mid-point (make-posn 0 0) (make-posn 10 44))
              (make-posn 5 22))

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define MT (empty-scene 400 400))
(define A (make-posn 200  50))
(define B (make-posn  27 350))
(define C (make-posn 373 350))
 
(add-sierpinski MT A B C)
