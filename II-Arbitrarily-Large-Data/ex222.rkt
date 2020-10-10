;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex222) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

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

; A DirInt (directional integer) is one of:
;     – -1,
;     –  1.
; Interpretation:
;     Represents the x-direction of a block,
;     i.e. -1 represents the negative x-direction (left)
;     and 1 represents the positive x-direction (right).

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

; Number -> Tetris
; Start the Tetris game by passing it the rate
; of clock ticks per r seconds.
(define (tetris-main r)
  (big-bang (make-tetris (make-block 0 0) empty)
    [on-draw tetris-render]
    [on-tick tock r]
    [on-key ke-handler]))

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

; Tetris -> Tetris
; Change the state of the world every
; clock tick by dropping the current block.
(define (tock t)
  (cond
    [(or (member? (drop-block (tetris-block t))
                  (tetris-landscape t))
         (block-landed? (tetris-block t)))
     (make-tetris (next-block (tetris-block t))
                  (cons (tetris-block t)
                        (tetris-landscape t)))]
    [else
     (make-tetris (drop-block (tetris-block t))
                  (tetris-landscape t))]))

(check-expect (tock (make-tetris
                     (make-block 0 (- HEIGHT 4))
                     landscape2))
              (make-tetris (make-block 0 (- HEIGHT 3))
                           landscape2))
(check-expect (tock (make-tetris
                     (make-block 0 (- HEIGHT 1))
                     '()))
              (make-tetris
               (make-block 1 0)
               (list (make-block 0 (- HEIGHT 1)))))

; Tetris KeyEvent -> Tetris
; Move the current block left or right
; if possible.
(define (ke-handler t ke)
  (cond
    [(key=? ke "left")
     (if (or (member? (move-block-sideways (tetris-block t) -1)
                      (tetris-landscape t))
             (outside-scene? (move-block-sideways (tetris-block t) -1)))
         t
         (make-tetris (move-block-sideways (tetris-block t) -1)
                      (tetris-landscape t)))]
    [(key=? ke "right")
     (if (or (member? (move-block-sideways (tetris-block t) 1)
                      (tetris-landscape t))
             (outside-scene? (move-block-sideways (tetris-block t) 1)))
         t
         (make-tetris (move-block-sideways (tetris-block t) 1)
                      (tetris-landscape t)))]
    [else t]))

(check-expect (ke-handler (make-tetris (make-block (* 1/2 WIDTH) (* 1/2 HEIGHT))
                                       '())
                          "left")
              (make-tetris (make-block (- (* 1/2 WIDTH) 1) (* 1/2 HEIGHT))
                           '()))
(check-expect (ke-handler (make-tetris (make-block 0 (* 1/2 HEIGHT))
                                       '())
                          "left")
              (make-tetris (make-block 0 (* 1/2 HEIGHT))
                           '()))
(check-expect (ke-handler (make-tetris (make-block 1 (- HEIGHT 2))
                                       (list (make-block 0 (- HEIGHT 2))
                                             (make-block 0 (- HEIGHT 1))))
                          "left")
              (make-tetris (make-block 1 (- HEIGHT 2))
                           (list (make-block 0 (- HEIGHT 2))
                                 (make-block 0 (- HEIGHT 1)))))

(check-expect (ke-handler (make-tetris (make-block (* 1/2 WIDTH) (* 1/2 HEIGHT))
                                       '())
                          "right")
              (make-tetris (make-block (+ (* 1/2 WIDTH) 1) (* 1/2 HEIGHT))
                           '()))
(check-expect (ke-handler (make-tetris (make-block (- WIDTH 1) (* 1/2 HEIGHT))
                                       '())
                          "right")
              (make-tetris (make-block (- WIDTH 1) (* 1/2 HEIGHT))
                           '()))
(check-expect (ke-handler (make-tetris (make-block (- WIDTH 2) (- HEIGHT 2))
                                       (list
                                        (make-block (- WIDTH 1) (- HEIGHT 2))
                                        (make-block (- WIDTH 1) (- HEIGHT 1))))
                          "right")
              (make-tetris (make-block (- WIDTH 2) (- HEIGHT 2))
                           (list (make-block (- WIDTH 1) (- HEIGHT 2))
                                 (make-block (- WIDTH 1) (- HEIGHT 1)))))

(check-expect (ke-handler tetris2 " ")
              tetris2)

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

; Block -> Boolean
; Has the block landed on the floor?
(define (block-landed? b)
  (>= (block-y b) (- HEIGHT 1)))

(check-expect (block-landed? (make-block 3 (- HEIGHT 2)))
              #false)
(check-expect (block-landed? (make-block 3 (- HEIGHT 1)))
              #true)
(check-expect (block-landed? (make-block 3 (- HEIGHT 0)))
              #true)

; Block -> Block
; Drop the block b.
(define (drop-block b)
  (make-block (block-x b)
              (+ (block-y b) 1)))

(check-expect (drop-block (make-block 0 0))
              (make-block 0 1))

; Block -> Block
; Create a new block that descends
; on the column to the right of the
; current one.
(define (next-block b)
  (make-block (if (>= (+ (block-x b) 1) WIDTH)
                  0
                  (+ (block-x b) 1))
              0))

(check-expect (next-block (make-block 0 0))
              (make-block 1 0))
(check-expect (next-block (make-block (- WIDTH 1) 5))
              (make-block 0 0))

; Block DirInt -> Block
; Move the block b in the direction
; specified by di.
(define (move-block-sideways b di)
  (make-block (+ (block-x b) di)
              (block-y b)))

(check-expect (move-block-sideways (make-block 5 5) -1)
              (make-block 4 5))
(check-expect (move-block-sideways (make-block 5 5) 1)
              (make-block 6 5))

; Block -> Boolean
; Is the block b outside the scene in x-direction?
(define (outside-scene? b)
  (or (< (block-x b) 0)
      (>= (block-x b) WIDTH)))

(check-expect (outside-scene? (make-block (* 1/2 WIDTH) (* 1/2 HEIGHT)))
              #false)
(check-expect (outside-scene? (make-block -1 (* 1/2 HEIGHT)))
              #true)
(check-expect (outside-scene? (make-block WIDTH (* 1/2 HEIGHT)))
              #true)
