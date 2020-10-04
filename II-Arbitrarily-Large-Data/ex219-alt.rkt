;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex219-alt) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define-struct dir [x y])
; A DirVec is a structure:
;     (make-dir N N)
; where N is one of -1, 0 or 1.
; Interpretation:
;     A directional vector giving the direction
;     of a point in the x- and y-direction.

(define-struct worm-seg [pos dir])
; A WormSegment is a structure:
;     (make-worm-seg Posn DirVec)
; Interpretation:
;     The current position of the worm segment
;     on the canvas and its directional vector.
;     The directional vector should only contain
;     the integers -1, 0 and 1.

; A List-of-Posn is one of:
; – '()
; – (cons Posn List-of-Posns)
; Interpretation:
;     A list with the positions of
;     certain elements like obstacles.

; A NE-Worm (non-empty worm) is one of:
; – (cons WormSegment '())
; – (cons WormSegment NE-Worm)
; Interpretation:
;     The segment(s) of the worm,
;     head last, tail first.

(define-struct ws [worm food obstacles])
; A WorldState is a structure:
;     (make-ws NE-Worm Posn List-of-Posn)
; Interpretation:
;     Contains a worm and the current
;     position of the food.

; CONSTANTS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define RADIUS 5)                          ; radius of one segment of the worm
(define WIDTH (min 1000 (* 40 RADIUS)))    ; width of the canvas
(define HEIGHT (min 1250 (* 52 RADIUS)))   ; height of the canvas
(define SPEED (* 2 RADIUS))                ; the speed of the worm
(define NUMBER-OF-OBSTACLES 5)             ; the number of obstacles in the game

(define WORM (circle RADIUS "solid" "red"))
(define FOOD (circle RADIUS "solid" "green"))
(define BLOCK (square (* 2 RADIUS) "solid" "Gainsboro"))
(define BACKGROUND (empty-scene WIDTH HEIGHT "Midnight Blue"))

(define MAX-X (/ WIDTH (image-width WORM)))
(define MAX-Y (/ HEIGHT (image-width WORM)))

(define NUMBER-ERROR (string-append "The number must be an integer between "
                                    "1 and 10 inclusive."))

(define GAME-OVER-OBSTACLE (text "worm hit obstacle" (* 4 RADIUS) "red"))
(define GAME-OVER-SELF (text "worm ran into itself" (* 4 RADIUS) "red"))
(define GAME-OVER-HEIGHT (max (image-height GAME-OVER-OBSTACLE)
                              (image-height GAME-OVER-SELF)))

; ------------------------------------------------------------------------------

; for testing
(define TEST-WORM-1
  (list
   (make-worm-seg
    (make-posn (+ (* 1/2 WIDTH) RADIUS)
               (- (* 1/2 HEIGHT) (* 9 RADIUS)))
    (make-dir 0 1))
   (make-worm-seg
    (make-posn (+ (* 1/2 WIDTH) RADIUS)
               (- (* 1/2 HEIGHT) (* 7 RADIUS)))
    (make-dir 0 1))
   (make-worm-seg
    (make-posn (+ (* 1/2 WIDTH) RADIUS)
               (- (* 1/2 HEIGHT) (* 5 RADIUS)))
    (make-dir 0 1))
   (make-worm-seg
    (make-posn (+ (* 1/2 WIDTH) RADIUS)
               (- (* 1/2 HEIGHT) (* 3 RADIUS)))
    (make-dir 0 1))   
   (make-worm-seg
    (make-posn (+ (* 1/2 WIDTH) RADIUS)
               (- (* 1/2 HEIGHT) RADIUS))
    (make-dir 0 1))))

(define TEST-WORM-2
  (list
   (make-worm-seg
    (make-posn (+ (* 1/2 WIDTH) RADIUS)
               (- (* 1/2 HEIGHT) (* 9 RADIUS)))
    (make-dir 0 -1))
   (make-worm-seg
    (make-posn (+ (* 1/2 WIDTH) RADIUS)
               (- (* 1/2 HEIGHT) (* 7 RADIUS)))
    (make-dir 0 -1))
   (make-worm-seg
    (make-posn (+ (* 1/2 WIDTH) RADIUS)
               (- (* 1/2 HEIGHT) (* 5 RADIUS)))
    (make-dir 0 -1))
   (make-worm-seg
    (make-posn (+ (* 1/2 WIDTH) RADIUS)
               (- (* 1/2 HEIGHT) (* 3 RADIUS)))
    (make-dir 0 -1))
   (make-worm-seg
    (make-posn (+ (* 1/2 WIDTH) RADIUS)
               (- (* 1/2 HEIGHT) RADIUS))
    (make-dir 0 -1))))


(define TEST-WORM-3
  (list
   (make-worm-seg
    (make-posn (+ (* 1/2 WIDTH) RADIUS)
               (- (* 1/2 HEIGHT) RADIUS))
    (make-dir 1 0))
   (make-worm-seg
    (make-posn (+ (* 1/2 WIDTH) RADIUS)
               (- (* 1/2 HEIGHT) (* 3 RADIUS)))
    (make-dir 1 0))
   (make-worm-seg
    (make-posn (+ (* 1/2 WIDTH) RADIUS)
               (- (* 1/2 HEIGHT) (* 5 RADIUS)))
    (make-dir 1 0))
   (make-worm-seg
    (make-posn (+ (* 1/2 WIDTH) RADIUS)
               (- (* 1/2 HEIGHT) (* 7 RADIUS)))
    (make-dir 1 0))
   (make-worm-seg
    (make-posn (+ (* 1/2 WIDTH) RADIUS)
               (- (* 1/2 HEIGHT) (* 9 RADIUS)))
    (make-dir 1 0))))

(define TEST-WORM-4
  (list
   (make-worm-seg
    (make-posn (+ (* 1/2 WIDTH) RADIUS)
               (- (* 1/2 HEIGHT) (* 9 RADIUS)))
    (make-dir -1 0))
   (make-worm-seg
    (make-posn (+ (* 1/2 WIDTH) RADIUS)
               (- (* 1/2 HEIGHT) (* 7 RADIUS)))
    (make-dir -1 0))
   (make-worm-seg
    (make-posn (+ (* 1/2 WIDTH) RADIUS)
               (- (* 1/2 HEIGHT) (* 5 RADIUS)))
    (make-dir -1 0))
   (make-worm-seg
    (make-posn (+ (* 1/2 WIDTH) RADIUS)
               (- (* 1/2 HEIGHT) (* 3 RADIUS)))
    (make-dir -1 0))
   (make-worm-seg
    (make-posn (+ (* 1/2 WIDTH) RADIUS)
               (- (* 1/2 HEIGHT) RADIUS))
    (make-dir -1 0))))

(define TEST-WORLDSTATE-1
  (make-ws
   TEST-WORM-1
   (make-posn 195 115)
   (list (make-posn 75 25)
         (make-posn 115 105)
         (make-posn 75 25)
         (make-posn 55 135)
         (make-posn 75 135))))

(define TEST-WORLDSTATE-2
  (make-ws
   TEST-WORM-2
   (make-posn 195 115)
   (list (make-posn 75 25)
         (make-posn 115 105)
         (make-posn 75 25)
         (make-posn 55 135)
         (make-posn 75 135))))

(define TEST-WORLDSTATE-3
  (make-ws
   TEST-WORM-3
   (make-posn 195 115)
   (list (make-posn 75 25)
         (make-posn 115 105)
         (make-posn 75 25)
         (make-posn 55 135)
         (make-posn 75 135))))

(define TEST-WORLDSTATE-4
  (make-ws
   TEST-WORM-4
   (make-posn 195 115)
   (list (make-posn 75 25)
         (make-posn 115 105)
         (make-posn 75 25)
         (make-posn 55 135)
         (make-posn 75 135))))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Number -> WorldState
; Start the program here.
; Pass it the time between clock ticks in seconds.
(define (worm-main s)
  (big-bang (create-valid-ws
             (make-ws (list
                       (make-worm-seg
                        (make-posn (+ (* 1/2 WIDTH) RADIUS)
                                   (+ (* 1/2 HEIGHT) RADIUS))
                        (make-dir 0 1)))
                      ; food create could be passed any posn, as it creates a
                      ; new one on each call using the passed posn only so that
                      ; the new food is not at the same posn as the one before
                      (food-create (make-posn (* -1 RADIUS) (* -1 RADIUS)))
                      (make-obstacles NUMBER-OF-OBSTACLES)))
    [on-draw render]
    [on-key ke-handler]
    [on-tick tock s]
    [stop-when game-over? render-final]))

; WorldState -> Image
; Render the current state of the
; world as an image.
(define (render ws)
  (render-worm (ws-worm ws)
               (place-image FOOD
                            (posn-x (ws-food ws))
                            (posn-y (ws-food ws))
                            (render-obstacles (ws-obstacles ws)
                                              BACKGROUND))))

(check-expect (render
               (make-ws
                (list (make-worm-seg (make-posn 75 100) (make-dir 1 0)))
                (make-posn 125 145)
                (list (make-posn 65 95)
                      (make-posn 75 65)
                      (make-posn 155 65))))
              (place-image
               WORM
               75
               100
               (place-image
                FOOD
                125
                145
                (render-obstacles
                 (list (make-posn 65 95)
                       (make-posn 75 65)
                       (make-posn 155 65))
                 BACKGROUND))))

(check-expect (render
               (make-ws
                (list
                 (make-worm-seg
                  (make-posn (+ (* 1/2 WIDTH) RADIUS)
                             (- (* 1/2 HEIGHT) RADIUS))
                  (make-dir 0 1))
                 (make-worm-seg
                  (make-posn (+ (* 1/2 WIDTH) RADIUS)
                             (- (* 1/2 HEIGHT) (* 3 RADIUS)))
                  (make-dir 0 1))
                 (make-worm-seg
                  (make-posn (+ (* 1/2 WIDTH) RADIUS)
                             (- (* 1/2 HEIGHT) (* 5 RADIUS)))
                  (make-dir 0 1)))
               (make-posn 125 125)
               (list (make-posn 65 95)
                     (make-posn 75 65)
                     (make-posn 155 65))))
               (place-image
                WORM
                (+ (* 1/2 WIDTH) RADIUS)
                (- (* 1/2 HEIGHT) RADIUS)
                (place-image
                 WORM
                 (+ (* 1/2 WIDTH) RADIUS)
                 (- (* 1/2 HEIGHT) (* 3 RADIUS))
                 (place-image
                  WORM
                  (+ (* 1/2 WIDTH) RADIUS)
                  (- (* 1/2 HEIGHT) (* 5 RADIUS))
                  (place-image
                   FOOD
                   125
                   125
                   (render-obstacles
                    (list (make-posn 65 95)
                          (make-posn 75 65)
                          (make-posn 155 65))
                    BACKGROUND))))))

; WorldState KeyEvent -> WorldState
; Change the direction of the worm according
; to the arrow key pressed.
(define (ke-handler ws ke)
  (cond
    [(key=? "up" ke)
     ; prohibits the worm from reversing direction and thus running into itself
     (if (equal? (get-head-dir (ws-worm ws))
                 (make-dir 0 1))
         ws
         (make-ws (set-head-dir (ws-worm ws) (make-dir 0 -1))
                  (ws-food ws)
                  (ws-obstacles ws)))]
    [(key=? "down" ke)
     (if (equal? (get-head-dir (ws-worm ws))
                 (make-dir 0 -1))
         ws
         (make-ws (set-head-dir (ws-worm ws) (make-dir 0 1))
                  (ws-food ws)
                  (ws-obstacles ws)))]
    [(key=? "right" ke)
     (if (equal? (get-head-dir (ws-worm ws))
                 (make-dir -1 0))
         ws
         (make-ws (set-head-dir (ws-worm ws) (make-dir 1 0))
                  (ws-food ws)
                  (ws-obstacles ws)))]
    [(key=? "left" ke)
     (if (equal? (get-head-dir (ws-worm ws))
                 (make-dir 1 0))
         ws
         (make-ws (set-head-dir (ws-worm ws) (make-dir -1 0))
                  (ws-food ws)
                  (ws-obstacles ws)))]))

; prohibit reversing direction
(check-expect (ke-handler TEST-WORLDSTATE-1 "up")
              TEST-WORLDSTATE-1)
(check-expect (ke-handler TEST-WORLDSTATE-3 "up")
              (make-ws
               (list
                (first TEST-WORM-3)
                (second TEST-WORM-3)
                (third TEST-WORM-3)
                (fourth TEST-WORM-3)
                (make-worm-seg
                 (worm-seg-pos (fifth TEST-WORM-3))
                 (make-dir 0 -1)))
               (ws-food TEST-WORLDSTATE-3)
               (ws-obstacles TEST-WORLDSTATE-3)))

(check-expect (ke-handler TEST-WORLDSTATE-2 "down")
              TEST-WORLDSTATE-2)
(check-expect (ke-handler TEST-WORLDSTATE-3 "down")
              (make-ws
               (list
                (first TEST-WORM-3)
                (second TEST-WORM-3)
                (third TEST-WORM-3)
                (fourth TEST-WORM-3)
                (make-worm-seg
                 (worm-seg-pos (fifth TEST-WORM-3))
                 (make-dir 0 1)))
               (ws-food TEST-WORLDSTATE-3)
               (ws-obstacles TEST-WORLDSTATE-3)))

(check-expect (ke-handler TEST-WORLDSTATE-4 "right")
              TEST-WORLDSTATE-4)
(check-expect (ke-handler TEST-WORLDSTATE-1 "right")
              (make-ws
               (list
                (first TEST-WORM-1)
                (second TEST-WORM-1)
                (third TEST-WORM-1)
                (fourth TEST-WORM-1)
                (make-worm-seg
                 (worm-seg-pos (fifth TEST-WORM-1))
                 (make-dir 1 0)))
               (ws-food TEST-WORLDSTATE-1)
               (ws-obstacles TEST-WORLDSTATE-1)))

(check-expect (ke-handler TEST-WORLDSTATE-3 "left")
              TEST-WORLDSTATE-3)
(check-expect (ke-handler TEST-WORLDSTATE-2 "left")
              (make-ws
               (list
                (first TEST-WORM-2)
                (second TEST-WORM-2)
                (third TEST-WORM-2)
                (fourth TEST-WORM-2)
                (make-worm-seg
                 (worm-seg-pos (fifth TEST-WORM-2))
                 (make-dir -1 0)))
               (ws-food TEST-WORLDSTATE-2)
               (ws-obstacles TEST-WORLDSTATE-2)))

; NOTE -------------------------------------------------------------------------

; The tock function must deal with two things:
;     – moving the worm,
;     – eating food and creating new food.
; So there are two cases:
;     1) the worm has to be moved, it is not eating food at the moment;
;     2) the worm has to be moved and has to grow, since it is eating / has
;        just eaten.
; This is reflected in the function logic.

; END NOTE ---------------------------------------------------------------------

; WorldState -> WorldState
(define (tock ws)
  (cond
    [(food-eaten? ws)
     (make-ws
      (append
       (ws-worm ws)
       (move-worm
        (list
         (get-head (ws-worm ws)))))
      (food-create (ws-food ws))
      (ws-obstacles ws))]
    [else
     (make-ws
      (move-worm (ws-worm ws))
      (ws-food ws)
      (ws-obstacles ws))]))

(check-expect (ws-worm
               (tock
                (make-ws
                 (list
                  (make-worm-seg
                   (make-posn (* 3 RADIUS) (* 3 RADIUS))
                   (make-dir 1 0))
                  (make-worm-seg
                   (make-posn (* 5 RADIUS) (* 3 RADIUS))
                   (make-dir 0 1))
                  (make-worm-seg
                   (make-posn (* 5 RADIUS) (* 5 RADIUS))
                   (make-dir 0 1)))
                 (make-posn (* 5 RADIUS) (* 5 RADIUS))
                 '())))
              (list
               (make-worm-seg
                (make-posn (* 3 RADIUS) (* 3 RADIUS))
                (make-dir 1 0))
               (make-worm-seg
                (make-posn (* 5 RADIUS) (* 3 RADIUS))
                (make-dir 0 1))
               (make-worm-seg
                (make-posn (* 5 RADIUS) (* 5 RADIUS))
                (make-dir 0 1))
               (make-worm-seg
                (make-posn (* 5 RADIUS) (* 7 RADIUS))
                (make-dir 0 1))))

(check-expect (tock
               (make-ws
                (list
                 (make-worm-seg
                  (make-posn (* 3 RADIUS) (* 3 RADIUS))
                  (make-dir 1 0))
                 (make-worm-seg
                  (make-posn (* 5 RADIUS) (* 3 RADIUS))
                  (make-dir 0 1))
                 (make-worm-seg
                  (make-posn (* 5 RADIUS) (* 5 RADIUS))
                  (make-dir 0 1)))
                (make-posn RADIUS RADIUS)
                '()))
              (make-ws
               (list
                (make-worm-seg
                  (make-posn (* 5 RADIUS) (* 3 RADIUS))
                  (make-dir 0 1))
                 (make-worm-seg
                  (make-posn (* 5 RADIUS) (* 5 RADIUS))
                  (make-dir 0 1))
                 (make-worm-seg
                  (make-posn (* 5 RADIUS) (* 7 RADIUS))
                  (make-dir 0 1)))
               (make-posn RADIUS RADIUS)
               '()))

; WorldState -> Boolean
(define (game-over? ws)
  (or (run-into-self? (ws-worm ws))
      (run-into-obstacle? ws)))

(check-expect (game-over? TEST-WORLDSTATE-1)
              #false)

; WorldState -> Image
(define (render-final ws)
  (place-image/align (cond
                       [(run-into-obstacle? ws)
                        GAME-OVER-OBSTACLE]
                       [(run-into-self? (ws-worm ws))
                        GAME-OVER-SELF])
                     (* 1 RADIUS)
                     (- (image-height BACKGROUND)
                        (* 1/2 GAME-OVER-HEIGHT)
                        (* 0 RADIUS))
                     "left"
                     "middle"
                     (render ws)))

; worm ran into itself
(check-expect (render-final (make-ws
                             (list
                              (make-worm-seg
                               (make-posn 15 15) (make-dir 1 0))
                              (make-worm-seg
                               (make-posn 25 15) (make-dir 0 1))
                              (make-worm-seg
                               (make-posn 25 25) (make-dir -1 0))
                              (make-worm-seg
                               (make-posn 15 25) (make-dir 0 -1))
                              (make-worm-seg
                               (make-posn 15 15) (make-dir 0 -1)))
                             (make-posn 55 55)
                             '()))
              (place-image/align
               GAME-OVER-SELF
               RADIUS
               (- (image-height BACKGROUND)
                  (* 1/2 GAME-OVER-HEIGHT))
               "left"
               "middle"
               (render
                (make-ws
                 (list
                  (make-worm-seg
                   (make-posn 15 15) (make-dir 1 0))
                  (make-worm-seg
                   (make-posn 25 15) (make-dir 0 1))
                  (make-worm-seg
                   (make-posn 25 25) (make-dir -1 0))
                  (make-worm-seg
                   (make-posn 15 25) (make-dir 0 -1))
                  (make-worm-seg
                   (make-posn 15 15) (make-dir 0 -1)))
                 (make-posn 55 55)
                 '()))))

; worm hit obstacle
(check-expect (render-final (make-ws
                             (list
                              (make-worm-seg
                               (make-posn 25 15) (make-dir 0 1))
                              (make-worm-seg
                               (make-posn 25 25) (make-dir -1 0))
                              (make-worm-seg
                               (make-posn 15 25) (make-dir 0 -1))
                              (make-worm-seg
                               (make-posn 15 15) (make-dir 0 -1)))
                             (make-posn 55 55)
                             (list
                              (make-posn 15 15))))
              (place-image/align
               GAME-OVER-OBSTACLE
               RADIUS
               (- (image-height BACKGROUND)
                  (* 1/2 GAME-OVER-HEIGHT))
               "left"
               "middle"
               (render
                (make-ws
                 (list
                  (make-worm-seg
                   (make-posn 25 15) (make-dir 0 1))
                  (make-worm-seg
                   (make-posn 25 25) (make-dir -1 0))
                  (make-worm-seg
                   (make-posn 15 25) (make-dir 0 -1))
                  (make-worm-seg
                   (make-posn 15 15) (make-dir 0 -1)))
                 (make-posn 55 55)
                 (list
                  (make-posn 15 15))))))

; FUNCTIONS TO CREATE FOOOD ++++++++++++++++++++++++++++++++++++++++++++++++++++

; Posn -> Posn 
; Create a random position for the food.
(define (food-create p)
  (food-check-create
     p (make-posn (random-diameter MAX-X) (random-diameter MAX-Y))))

(check-satisfied (food-create (make-posn 1 1))
                 divisible-by-RADIUS-and-within-canvas?)

; Posn Posn -> Posn 
; generative recursion 
; Create a random position for the food
; that is different from the previous
; position.
(define (food-check-create p candidate)
  (if (equal? p candidate) (food-create p) candidate))
 
; Posn -> Boolean
; Are the coordinates in p divisible by 5
; and withing the canvas?
(define (divisible-by-RADIUS-and-within-canvas? p)
  (and (<= RADIUS (posn-x p))
       (>= (- WIDTH RADIUS) (posn-x p))
       (<= RADIUS (posn-y p))
       (>= (- HEIGHT RADIUS) (posn-y p))
       (= (modulo (posn-x p) RADIUS) 0)
       (= (modulo (posn-y p) RADIUS) 0)))

; N -> N
; Return a random number that is
; divisible by RADIUS but not (* 2 RADIUS) and less than
; (* 10 n) and greater or equal than RADIUS.
(define (random-diameter n)
  (- (random-2r n) RADIUS))

(check-satisfied (random-diameter MAX-X)
                 divisible-by-RADIUS-but-not-2RADIUS-and-within-canvas?)

; N -> N
; Return a random number x that is divisible by (* 2 RADIUS)
; and (* 2 RADIUS) <= x <= (* (* 2 RADIUS) n).
(define (random-2r n)
  (* (* RADIUS 2) (+ (random n) 1)))

(check-satisfied (random-2r MAX-X)
                 divisible-by-2RADIUS-and-within-canvas?)

; N -> Boolean
; Is n divisible by RADIUS and RADIUS <= n <= (- WIDTH RADIUS)?
(define (divisible-by-RADIUS-but-not-2RADIUS-and-within-canvas? n)
  (and (>= n RADIUS)
       (<= n (- WIDTH RADIUS))
       (= (modulo n RADIUS) 0)))

(check-expect (divisible-by-RADIUS-but-not-2RADIUS-and-within-canvas? RADIUS)
              #true)
(check-expect (divisible-by-RADIUS-but-not-2RADIUS-and-within-canvas?
               (+ RADIUS 1))
              #false)
(check-expect (divisible-by-RADIUS-but-not-2RADIUS-and-within-canvas?
               (- RADIUS 1))
              #false)
(check-expect (divisible-by-RADIUS-but-not-2RADIUS-and-within-canvas?
               (+ (- WIDTH RADIUS) 1))
              #false)
(check-expect (divisible-by-RADIUS-but-not-2RADIUS-and-within-canvas?
               (- (- WIDTH RADIUS) 1))
              #false)
(check-expect (divisible-by-RADIUS-but-not-2RADIUS-and-within-canvas?
               (- WIDTH RADIUS))
              #true)
(check-expect (divisible-by-RADIUS-but-not-2RADIUS-and-within-canvas?
               (* 10 RADIUS))
              #true)

; N -> Boolean
; Is n divisible by (* 2 RADIUS)
; and (* 2 RADIUS) <= x <= (* (* 2 RADIUS) n)?
(define (divisible-by-2RADIUS-and-within-canvas? n)
  (and (>= n (* 2 RADIUS))
       (<= n WIDTH)
       (= (modulo n (* 2 RADIUS)) 0)))

; NOTE -------------------------------------------------------------------------

; The create-valid-ws function is used so that not food is placed
; on an obstacle.

; END NOTE ---------------------------------------------------------------------

; WorldState -> WorldState
; Make sure the food is not in the same place
; as one of the obstacles.
(define (create-valid-ws ws)
  (make-ws (ws-worm ws)
           (compare-food-obstacles (ws-food ws) (ws-obstacles ws))
           (ws-obstacles ws)))

(check-expect (create-valid-ws
               (make-ws
                (list (make-posn 15 15)
                      (make-posn 15 25)
                      (make-posn 15 35))
                (make-posn 105 105)
                (list (make-posn 45 65)
                      (make-posn 25 25)
                      (make-posn 75 65))))
              (make-ws
               (list (make-posn 15 15)
                     (make-posn 15 25)
                     (make-posn 15 35))
               (make-posn 105 105)
               (list (make-posn 45 65)
                     (make-posn 25 25)
                     (make-posn 75 65))))

; NOTE -------------------------------------------------------------------------

; Uses recursion that is not based on data but on logic, i.e. it does not follow
; the design recipe as introduced so far.
; I also do not know how to test this function.

; END NOTE ---------------------------------------------------------------------

; Posn List-of-Posn -> Posn
; Create a new position for the food f
; if it is in the same place as an obstacle in loo.
(define (compare-food-obstacles f loo)
  (cond
    [(empty? loo) f]
    [(cons? loo)
     (if (member? f loo)
         (compare-food-obstacles (food-create f) loo)
         f)]))

; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

; FUNCTIONS TO CREATE OBSTACLES ++++++++++++++++++++++++++++++++++++++++++++++++

; NOTE -------------------------------------------------------------------------

; This function (make-obstacles) will always return a list with length n, but
; might contain duplicates. To prevent this, one would have to remove the
; duplicates and then compare the length of the result with the passed number n.
; If the length and n were not equal, one would have to append a list with the
; missing number of elements to the result.
; This approach demands and if / or cond construct where one would create a
; result in the "question" part, and if that question would result false,
; another result to create the final result. This second result would be
; different from the first one, because of the use of random, and would negate
; the whole effort to remove duplicates and create a list with the intended
; length.

; END NOTE ---------------------------------------------------------------------

; N -> List-of-Posn
; Create a list with random positions
; of n obstacles.
(define (make-obstacles n)
  (cond
    [(zero? n) '()]
    [else
     (cons
      (make-posn (random-obstacle MAX-X)
                 (random-obstacle MAX-Y))
      (make-obstacles (sub1 n)))]))

; List-of-Posn N -> List-of-Posn
; Make sure that the list of obstacles contains
; no duplicates and is as long as demanded.
(define (enough-obstacles lop n)
  (cond
    [(= (length (remove-duplicates lop)) n)
     (remove-duplicates lop)]
    [(< (length (remove-duplicates lop)) n)
     (enough-obstacles
      (append lop
              (make-obstacles
               (- n (length (remove-duplicates lop)))))
      n)]))

(check-expect (length (enough-obstacles (make-obstacles 100) 100))
              100)

; List-of-Posn -> List-of-Posn
; Remove all duplicates from lop.
(define (remove-duplicates lop)
  (cond
    [(empty? lop) '()]
    [(cons? lop)
     (if (member? (first lop) (rest lop))
         (remove-duplicates (rest lop))
         (cons (first lop)
               (remove-duplicates (rest lop))))]))

(check-expect (remove-duplicates
               (list
                (make-posn 15 15)
                (make-posn 55 67)
                (make-posn 15 15)))
              (list
               (make-posn 55 67)
               (make-posn 15 15)))

; N -> N
; Return a random number x that is divisible
; by RADIUS, but not (* 2 RADIUS) and
; (* 2 RADIUS) < x < (- n (* 2 RADIUS)).
(define (random-obstacle n)
  (+ (* (+ (random (- n 3))
           1)
        (* 2 RADIUS))
     RADIUS))

(check-satisfied (random-obstacle 20)
                 >=15-<=185-and-divisible-by-RADIUS-but-not-by-2RADIUS?)

; N -> Boolean
; Is x divisible by RADIUS, but not (* 2 RADIUS) and
; (* 2 RADIUS) < x ?
(define (>=15-<=185-and-divisible-by-RADIUS-but-not-by-2RADIUS? x)
  (and (>= x 15)
       (<= x 185)
       (zero? (modulo x RADIUS))
       (not (zero? (modulo x (* 2 RADIUS))))))

(check-expect (>=15-<=185-and-divisible-by-RADIUS-but-not-by-2RADIUS? 15)
              #true)
(check-expect (>=15-<=185-and-divisible-by-RADIUS-but-not-by-2RADIUS? 20)
              #false)

; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

; NE-Worm Image -> Image
; Render the worm and place it in the image img.
(define (render-worm new img)
  (cond
    [(empty? (rest new))
     (place-image WORM
                  (worm-x (first new))
                  (worm-y (first new))
                  img)]
    [else
     (place-image WORM
                  (worm-x (first new))
                  (worm-y (first new))
                  (render-worm (rest new) img))]))

(check-expect (render-worm TEST-WORM-1 BACKGROUND)
              (place-image
               WORM
               (worm-x (first TEST-WORM-1))
               (worm-y (first TEST-WORM-1))
               (place-image
                WORM
                (worm-x (second TEST-WORM-1))
                (worm-y (second TEST-WORM-1))
                (place-image
                 WORM
                 (worm-x (third TEST-WORM-1))
                 (worm-y (third TEST-WORM-1))
                 (place-image
                  WORM
                  (worm-x (fourth TEST-WORM-1))
                  (worm-y (fourth TEST-WORM-1))
                  (place-image
                   WORM
                   (worm-x (fifth TEST-WORM-1))
                   (worm-y (fifth TEST-WORM-1))
                   BACKGROUND))))))

; List-of-Posn -> Image
; Render BLOCKS at the positions given in lop
; into the image img.
(define (render-obstacles lop img)
  (cond
    [(empty? lop) img]
    [(cons? lop)
     (place-image BLOCK
                  (posn-x (first lop))
                  (posn-y (first lop))
                  (render-obstacles (rest lop) img))]))

(check-expect (render-obstacles (list (make-posn 65 95)
                                      (make-posn 75 65)
                                      (make-posn 155 65))
                                BACKGROUND)
              (place-image
               BLOCK
               65
               95
               (place-image
                BLOCK
                75
                65
                (place-image
                 BLOCK
                 155
                 65
                 BACKGROUND))))

; WorldState -> Boolean
; Has the worm eaten the food?
(define (food-eaten? ws)
  (equal? (get-head-posn (ws-worm ws))
          (ws-food ws)))

(check-expect (food-eaten? TEST-WORLDSTATE-2)
              #false)
(check-expect (food-eaten? (make-ws
                            TEST-WORM-2
                            (worm-seg-pos (fifth TEST-WORM-2))
                            (ws-obstacles TEST-WORLDSTATE-2)))
              #true)

; NE-Worm -> NE-Worm
; Change the position of the worm every clock tick
; according to every segments directional vector.
; Change the directional vectors so that the segments
; follow each other.
; If the worm crosses the border of the canvas,
; make it reappear on the other side of the canvas.
(define (move-worm new)
  (cond
    [(empty? (rest new))
     (list
      (make-worm-seg
       (make-posn
        (normalize-x
         (+ (worm-x (first new)) (* (worm-x-dir (first new)) SPEED)))
        (normalize-y
         (+ (worm-y (first new)) (* (worm-y-dir (first new)) SPEED))))
       (worm-seg-dir (first new))))]
    [else
     (append
      (list
       (make-worm-seg
        (make-posn
         (normalize-x
          (+ (worm-x (first new)) (* (worm-x-dir (first new)) SPEED)))
         (normalize-y
          (+ (worm-y (first new)) (* (worm-y-dir (first new)) SPEED))))
        (worm-seg-dir (second new))))
       (move-worm (rest new)))]))

; move the worm in the middle of the canvas
(check-expect (move-worm (list
                          (make-worm-seg
                           (make-posn 65 65) (make-dir 0 1))
                          (make-worm-seg
                           (make-posn 55 65) (make-dir 1 0))
                          (make-worm-seg
                           (make-posn 55 55) (make-dir 0 1))))
              (list
               (make-worm-seg
                (make-posn 65 (+ 65 (* 1 SPEED))) (make-dir 1 0))
               (make-worm-seg
                (make-posn (+ 55 (* 1 SPEED)) 65) (make-dir 0 1))
               (make-worm-seg
                (make-posn 55 (+ 55 (* 1 SPEED))) (make-dir 0 1))))

; worm is about to leave the canvas at the top
(check-expect (move-worm (list
                          (make-worm-seg
                           (make-posn 85 (* 3 RADIUS)) (make-dir 0 -1))
                          (make-worm-seg
                           (make-posn 85 RADIUS) (make-dir 1 0))
                          (make-worm-seg
                           (make-posn 95 RADIUS) (make-dir 0 -1))))
              (list
               (make-worm-seg
                (make-posn 85 RADIUS) (make-dir 1 0))
               (make-worm-seg
                (make-posn 95 RADIUS) (make-dir 0 -1))
               (make-worm-seg
                (make-posn 95 (- HEIGHT RADIUS)) (make-dir 0 -1))))

; worm is about to leave the canvas at the right
(check-expect (move-worm (list
                          (make-worm-seg
                           (make-posn (- WIDTH (* 3 RADIUS)) 145)
                           (make-dir 0 -1))
                          (make-worm-seg
                           (make-posn (- WIDTH (* 3 RADIUS)) 135)
                           (make-dir 1 0))
                          (make-worm-seg
                           (make-posn (- WIDTH RADIUS) 135)
                           (make-dir 1 0))))
              (list
               (make-worm-seg
                (make-posn (- WIDTH (* 3 RADIUS)) 135) (make-dir 1 0))
               (make-worm-seg
                (make-posn (- WIDTH RADIUS) 135) (make-dir 1 0))
               (make-worm-seg
                (make-posn RADIUS 135) (make-dir 1 0))))

; worm is about to leave the canvas at the bottom
(check-expect (move-worm (list
                          (make-worm-seg
                           (make-posn 115 (- HEIGHT (* 3 RADIUS)))
                           (make-dir -1 0))
                          (make-worm-seg
                           (make-posn 105 (- HEIGHT (* 3 RADIUS)))
                           (make-dir 0 1))
                          (make-worm-seg
                           (make-posn 105 (- HEIGHT RADIUS))
                           (make-dir 0 1))))
              (list
               (make-worm-seg
                (make-posn 105 (- HEIGHT (* 3 RADIUS))) (make-dir 0 1))
               (make-worm-seg
                (make-posn 105 (- HEIGHT RADIUS)) (make-dir 0 1))
               (make-worm-seg
                (make-posn 105 RADIUS) (make-dir 0 1))))

; worm is about to leave the canvas at the left
(check-expect (move-worm (list
                          (make-worm-seg
                           (make-posn (* 3 RADIUS) 115) (make-dir 0 1))
                          (make-worm-seg
                           (make-posn (* 3 RADIUS) 125) (make-dir -1 0))
                          (make-worm-seg
                           (make-posn RADIUS 125) (make-dir -1 0))))
              (list
               (make-worm-seg
                (make-posn (* 3 RADIUS) 125) (make-dir -1 0))
               (make-worm-seg
                (make-posn RADIUS 125) (make-dir -1 0))
               (make-worm-seg
                (make-posn (- WIDTH RADIUS) 125) (make-dir -1 0))))

; NE-Worm -> WormSegment
(define (get-head new)
  (cond
    [(empty? (rest new)) (first new)]
    [else
     (get-head (rest new))]))

(check-expect (get-head TEST-WORM-1)
              (fifth TEST-WORM-1))

; NE-Worm -> NE-Worm
; Set the directional vector of the head
; of new to dirvec.
(define (set-head-dir new dirvec)
  (cond
    [(empty? (rest new))
     (list
      (make-worm-seg
       (worm-seg-pos (first new))
       dirvec))]
    [else
     (append
      (list (first new))
      (set-head-dir (rest new) dirvec))]))

(check-expect (set-head-dir TEST-WORM-1 (make-dir 0 -1))
              (list
               (make-worm-seg
                (worm-seg-pos (first TEST-WORM-1))
                (worm-seg-dir (first TEST-WORM-1)))
               (make-worm-seg
                (worm-seg-pos (second TEST-WORM-1))
                (worm-seg-dir (second TEST-WORM-1)))
               (make-worm-seg
                (worm-seg-pos (third TEST-WORM-1))
                (worm-seg-dir (third TEST-WORM-1)))
               (make-worm-seg
                (worm-seg-pos (fourth TEST-WORM-1))
                (worm-seg-dir (fourth TEST-WORM-1)))
               (make-worm-seg
                (worm-seg-pos (fifth TEST-WORM-1))
                (make-dir 0 -1))))

; NE-Worm -> DirVec
; Return the directional vector of the worms head,
; i.e. the directional vector of the last worm segment.
(define (get-head-dir new)
  (cond
    [(empty? (rest new))
     (worm-seg-dir (first new))]
    [else
     (get-head-dir (rest new))]))

(check-expect (get-head-dir
               (list
                (make-worm-seg (make-posn 15 35) (make-dir 1 0))
                (make-worm-seg (make-posn 25 35) (make-dir 0 -1))
                (make-worm-seg (make-posn 25 25) (make-dir -1 0))))
              (make-dir -1 0))

; WormSegment -> Number
; Return the current x-direction
; of the worm segment ws.
(define (worm-x-dir ws)
  (dir-x (worm-seg-dir ws)))

(check-expect (worm-x-dir (make-worm-seg (make-posn 75 100) (make-dir 1 0)))
              1)

; WormSegment -> Number
; Return the current y-direction
; of the worm segment ws.
(define (worm-y-dir ws)
  (dir-y (worm-seg-dir ws)))

(check-expect (worm-y-dir (make-worm-seg (make-posn 75 100) (make-dir 1 0)))
              0)

; NE-Worm -> Posn
; Return the current position of
; the worm's head.
(define (get-head-posn new)
  (cond
    [(empty? (rest new))
     (worm-seg-pos (first new))]
    [else
     (get-head-posn (rest new))]))

(check-expect (get-head-posn TEST-WORM-1)
              (make-posn (+ (* 1/2 WIDTH) RADIUS)
                         (- (* 1/2 HEIGHT) RADIUS)))

; NE-Worm -> List-of-Posns
; Return a list of the positions of the worm segments.
(define (get-worm-posns new)
  (cond
    [(empty? (rest new))
     (list (worm-seg-pos (first new)))]
    [else
     (append
      (list (worm-seg-pos (first new)))
      (get-worm-posns (rest new)))]))

(check-expect (get-worm-posns (list
                               (make-worm-seg (make-posn 10 10)
                                              (make-dir 1 0))))
              (list (make-posn 10 10)))
(check-expect (get-worm-posns TEST-WORM-1)
              (list
               (make-posn (+ (* 1/2 WIDTH) RADIUS)
                          (- (* 1/2 HEIGHT) (* 9 RADIUS)))
               (make-posn (+ (* 1/2 WIDTH) RADIUS)
                          (- (* 1/2 HEIGHT) (* 7 RADIUS)))
               (make-posn (+ (* 1/2 WIDTH) RADIUS)
                          (- (* 1/2 HEIGHT) (* 5 RADIUS)))
               (make-posn (+ (* 1/2 WIDTH) RADIUS)
                          (- (* 1/2 HEIGHT) (* 3 RADIUS)))
               (make-posn (+ (* 1/2 WIDTH) RADIUS)
                          (- (* 1/2 HEIGHT) RADIUS))))
               

; WormSegment -> Number
; Return the current x-position
; of the worm segment ws.
(define (worm-x ws)
  (posn-x (worm-seg-pos ws)))

(check-expect (worm-x (make-worm-seg (make-posn 75 100) (make-dir 1 0)))
              75)

; WormSegment -> Number
; Return the current y-position
; of the worm segment ws.
(define (worm-y ws)
  (posn-y (worm-seg-pos ws)))

(check-expect (worm-y (make-worm-seg (make-posn 75 100) (make-posn 1 0)))
              100)

; Number -> Number
; Make sure that x is within the canvas,
; i.e. if the coordinate is beyond the canvas,
; move it to the opposite site of the canvas.
(define (normalize-x x)
  (cond
    [(> x (- WIDTH RADIUS)) RADIUS]
    [(< x RADIUS) (- WIDTH RADIUS)]
    [else x]))

(check-expect (normalize-x (- RADIUS 1))
              (- WIDTH RADIUS))
(check-expect (normalize-x (* 1/2 WIDTH))
              (* 1/2 WIDTH))
(check-expect (normalize-x (- WIDTH 1))
              RADIUS)

; Number -> Number
; Make sure that y is within the canvas,
; i.e. if the coordinate is beyond the canvas,
; move it to the opposite site of the canvas.
(define (normalize-y y)
  (cond
    [(> y (- HEIGHT RADIUS)) RADIUS]
    [(< y RADIUS) (- HEIGHT RADIUS)]
    [else y]))

(check-expect (normalize-y (- RADIUS 1))
              (- HEIGHT RADIUS))
(check-expect (normalize-y (* 1/2 HEIGHT))
              (* 1/2 HEIGHT))
(check-expect (normalize-y (- HEIGHT 1))
              RADIUS)

; NE-Worm -> Boolean
; Has the worm run into itself?
(define (run-into-self? new)
  (member? (get-head-posn new)
           (remove (get-head-posn new)
                   (get-worm-posns new))))

(check-expect (run-into-self? TEST-WORM-1)
              #false)
(check-expect (run-into-self?
               (list
                (make-worm-seg
                 (make-posn 15 15) (make-dir 1 0))
                (make-worm-seg
                 (make-posn 25 15) (make-dir 0 1))
                (make-worm-seg
                 (make-posn 25 25) (make-dir -1 0))
                (make-worm-seg
                 (make-posn 15 25) (make-dir 0 -1))
                (make-worm-seg
                 (make-posn 15 15) (make-dir 0 -1))))
              #true)

; WorldState -> Boolean
; Has the worm run into an obstacle?
(define (run-into-obstacle? ws)
  (member? (get-head-posn (ws-worm ws))
           (ws-obstacles ws)))

(check-expect (run-into-obstacle?
               (make-ws
                (list
                 (make-worm-seg
                  (make-posn 15 15) (make-dir 1 0))
                 (make-worm-seg
                  (make-posn 25 15) (make-dir 1 0))
                 (make-worm-seg
                  (make-posn 35 15) (make-dir 1 0)))
                '()
                (list (make-posn 55 55))))
              #false)

(check-expect (run-into-obstacle?
               (make-ws
                (list
                 (make-worm-seg
                  (make-posn 15 15) (make-dir 1 0))
                 (make-worm-seg
                  (make-posn 25 15) (make-dir 1 0))
                 (make-worm-seg
                  (make-posn 35 15) (make-dir 1 0)))
                '()
                (list (make-posn 35 15))))
              #true)
  