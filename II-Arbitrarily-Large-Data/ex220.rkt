;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex220) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define-struct tetris [block landscape])
(define-struct block [x y])
 
; A Tetris is a structure:
;   (make-tetris Block Landscape)
; A Landscape is one of: 
; – '() 
; – (cons Block Landscape)
; A Block is a structure:
;   (make-block N N)
 
; interpretations
; (make-block x y) depicts a block whose left 
; corner is (* x SIZE) pixels from the left and
; (* y SIZE) pixels from the top;
; (make-tetris b0 (list b1 b2 ...)) means b0 is the
; dropping block, while b1, b2, and ... are resting

; CONSTANTS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define WIDTH 10) ; # of blocks, horizontally
(define HEIGHT 20) ; # of blocks, vertically
(define SIZE 10) ; blocks are squares
(define SCENE-WIDTH (* WIDTH SIZE))
(define SCENE-HEIGHT (* HEIGHT SIZE))
 
(define BLOCK ; red squares with black rims
  (overlay
    (square (- SIZE 1) "solid" "red")
    (square SIZE "outline" "black")))

(define SCENE (empty-scene SCENE-WIDTH SCENE-HEIGHT "Midnight Blue"))

; for testing ..................................................................

(define landscape0 '())
(define block-dropping (make-block 5 10))
(define tetris0 (make-tetris (make-block 0 3) landscape0))
(define tetris0-drop (make-tetris block-dropping landscape0))

(define block-landed (make-block 0 (- HEIGHT 1)))
(define landscape1 (list block-landed))
(define tetris1 (make-tetris block-dropping landscape1))

(define block-on-block (make-block 0 (- HEIGHT 2)))
(define landscape2 (list block-on-block block-landed))
(define tetris2 (make-tetris block-dropping landscape2))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Tetris -> Image
; Turn a given instance of t into
; an image.
(define (tetris-render t)
  (render-block (tetris-block t)
                (render-landscape (tetris-landscape t)
                                  SCENE)))

(check-expect (tetris-render tetris0)
              (place-image
               BLOCK
               (+ (* 0 SIZE) (* 1/2 SIZE))
               (+ (* 3 SIZE) (* 1/2 SIZE))
               SCENE))

(check-expect (tetris-render tetris0-drop)
              (place-image
               BLOCK
               (+ (* 5 SIZE) (* 1/2 SIZE))
               (+ (* 10 SIZE) (* 1/2 SIZE))
               SCENE))

(check-expect (tetris-render tetris1)
              (place-image
               BLOCK
               (+ (* 5 SIZE) (* 1/2 SIZE))
               (+ (* 10 SIZE) (* 1/2 SIZE))
               (place-image
                BLOCK
                (+ (* 0 SIZE) (* 1/2 SIZE))
                (+ (* (- HEIGHT 1) SIZE) (* 1/2 SIZE))
                SCENE)))

(check-expect (tetris-render tetris2)
              (place-image
               BLOCK
               (+ (* 5 SIZE) (* 1/2 SIZE))
               (+ (* 10 SIZE) (* 1/2 SIZE))
               (place-image
                BLOCK
                (+ (* 0 SIZE) (* 1/2 SIZE))
                (+ (* (- HEIGHT 2) SIZE) (* 1/2 SIZE))
                (place-image
                 BLOCK
                 (+ (* 0 SIZE) (* 1/2 SIZE))
                 (+ (* (- HEIGHT 1) SIZE) (* 1/2 SIZE))
                 SCENE))))

; Block Image -> Image
; Render block b in image img.
(define (render-block b img)
  (place-image BLOCK
               (+ (* (block-x b) SIZE) (* 1/2 SIZE))
               (+ (* (block-y b) SIZE) (* 1/2 SIZE))
               img))

; Landscape Image -> Image
; Render all elements of the landscape l
; in the image img.
(define (render-landscape l img)
  (cond
    [(empty? l) img]
    [(cons? l)
     (render-block (first l)
                   (render-landscape (rest l)
                                     img))]))