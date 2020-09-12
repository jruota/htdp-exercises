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

(define-struct worm [pos dir])
; A WormSegment is a structure:
;     (make-worm Posn DirVec)
; Interpretation:
;     The current position of the worm segment
;     on the canvas and its directional vector.
;     The directional vector should only contain
;     the integers -1, 0 and 1.

; A List-of-Posns is one of:
; – '()
; – (cons Posn List-of-Posns)
; Interpretation:
;     A list with the positions of the
;     worm segments.

; A NE-Worm (non-empty worm) is one of:
; – (cons WormSegment '())
; – (cons WormSegment NE-Worm)
; Interpretation:
;     The segment(s) of the worm,
;     head first, tail last.

(define-struct ws [worm food])
; A WorldState is a structure:
;     (make-ws NE-Worm Posn)
; Interpretation:
;     Contains a worm and the current
;     position of the food.

; CONSTANTS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define RADIUS 5)                          ; radius of one segment of the worm
(define WIDTH (min 1000 (* 40 RADIUS)))    ; width of the canvas
(define HEIGHT (min 1250 (* 52 RADIUS)))   ; height of the canvas
(define SPEED (* 2 RADIUS))                ; the speed of the worm

(define WORM (circle RADIUS "solid" "red"))
(define FOOD (circle RADIUS "solid" "green"))
(define BACKGROUND (empty-scene WIDTH HEIGHT "Midnight Blue"))

(define MAX-X (/ WIDTH (image-width WORM)))
(define MAX-Y (/ HEIGHT (image-width WORM)))

(define NUMBER-ERROR (string-append "The number must be an integer between "
                                    "1 and 10 inclusive."))

(define GAME-OVER-WALLS (text "worm hit border" (* 4 RADIUS) "red"))
(define GAME-OVER-SELF (text "worm ran into itself" (* 4 RADIUS) "red"))
(define GAME-OVER-HEIGHT (max (image-height GAME-OVER-WALLS)
                              (image-height GAME-OVER-SELF)))

; ------------------------------------------------------------------------------

; for testing
(define TEST-WORM
  (list (make-worm (make-posn (+ (* 1/2 WIDTH) RADIUS)
                              (- (* 1/2 HEIGHT) RADIUS))
                   (make-dir 0 0))
        (make-worm (make-posn (+ (* 1/2 WIDTH) RADIUS)
                              (- (* 1/2 HEIGHT) (* 3 RADIUS)))
                   (make-dir 0 0))
        (make-worm (make-posn (+ (* 1/2 WIDTH) RADIUS)
                              (- (* 1/2 HEIGHT) (* 5 RADIUS)))
                   (make-dir 0 0))
        (make-worm (make-posn (+ (* 1/2 WIDTH) RADIUS)
                              (- (* 1/2 HEIGHT) (* 7 RADIUS)))
                   (make-dir 0 0))
        (make-worm (make-posn (+ (* 1/2 WIDTH) RADIUS)
                              (- (* 1/2 HEIGHT) (* 9 RADIUS)))
                   (make-dir 0 0))))

(define TEST-WORM-2
  (list (make-worm (make-posn (+ (* 1/2 WIDTH) RADIUS)
                              (- (* 1/2 HEIGHT) RADIUS))
                   (make-dir 0 1))
        (make-worm (make-posn (+ (* 1/2 WIDTH) RADIUS)
                              (- (* 1/2 HEIGHT) (* 3 RADIUS)))
                   (make-dir 0 1))
        (make-worm (make-posn (+ (* 1/2 WIDTH) RADIUS)
                              (- (* 1/2 HEIGHT) (* 5 RADIUS)))
                   (make-dir 0 1))
        (make-worm (make-posn (+ (* 1/2 WIDTH) RADIUS)
                              (- (* 1/2 HEIGHT) (* 7 RADIUS)))
                   (make-dir 0 1))
        (make-worm (make-posn (+ (* 1/2 WIDTH) RADIUS)
                              (- (* 1/2 HEIGHT) (* 9 RADIUS)))
                   (make-dir 0 1))))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Number -> NE-WorldState
; Start the program here.
; Pass it the time between clock ticks in seconds
; and the number of worm segments.
(define (worm-main s n)
  (big-bang (make-ws (create-worm n)
                     (food-create (make-posn (* -1 RADIUS) (* -1 RADIUS))))
    [on-draw render]
    [on-key ke-handler]
    [on-tick move-worm s]
    [stop-when game-over? render-final]))
    ;[state #true]))

; WorldState -> Image
; Render the current state of the
; world as an image.
(define (render ws)
  (render-worm (ws-worm ws)
               (place-image FOOD
                            (posn-x (ws-food ws))
                            (posn-y (ws-food ws))
                            BACKGROUND)))

(check-expect (render
               (make-ws
                (list (make-worm (make-posn 75 100) (make-dir 1 0)))
                (make-posn 125 145)))
              (place-image WORM
                           75
                           100
                           (place-image FOOD
                                        125
                                        145
                                        BACKGROUND)))
(check-expect (render
               (make-ws
                (list
                 (make-worm
                  (make-posn (+ (* 1/2 WIDTH) RADIUS)
                             (- (* 1/2 HEIGHT) (* 5 RADIUS)))
                  (make-dir 0 0))
                 (make-worm
                  (make-posn (+ (* 1/2 WIDTH) RADIUS)
                             (- (* 1/2 HEIGHT) (* 3 RADIUS)))
                  (make-dir 0 0))
                 (make-worm
                  (make-posn (+ (* 1/2 WIDTH) RADIUS)
                             (- (* 1/2 HEIGHT) RADIUS))
                  (make-dir 0 0)))
               (make-posn 105 105)))
               (place-image
                WORM
                (+ (* 1/2 WIDTH) RADIUS)
                (- (* 1/2 HEIGHT) (* 5 RADIUS))
                (place-image
                 WORM
                 (+ (* 1/2 WIDTH) RADIUS)
                 (- (* 1/2 HEIGHT) (* 3 RADIUS))
                 (place-image
                  WORM
                  (+ (* 1/2 WIDTH) RADIUS)
                  (- (* 1/2 HEIGHT) RADIUS)
                  (place-image
                   FOOD
                   105
                   105
                   BACKGROUND)))))

; WorldState KeyEvent -> NE-WorldState
; Change the direction of the worm according
; to the arrow key pressed.
(define (ke-handler ws ke)
  (cond
    [(key=? ke "up")
     (if (dir-not-set? (ws-worm ws))
         (make-ws (set-dir (ws-worm ws) (make-dir 0 -1))
                  (ws-food ws))
         (make-ws (set-head-dir (ws-worm ws) (make-dir 0 -1))
                  (ws-food ws)))]
    [(key=? ke "down")
     (if (dir-not-set? (ws-worm ws))
         (make-ws (set-dir (ws-worm ws) (make-dir 0 1))
                  (ws-food ws))
         (make-ws (set-head-dir (ws-worm ws) (make-dir 0 1))
                  (ws-food ws)))]
    [(key=? ke "right")
     (if (dir-not-set? (ws-worm ws))
         (make-ws (set-dir (ws-worm ws) (make-dir 1 0))
                  (ws-food ws))
         (make-ws (set-head-dir (ws-worm ws) (make-dir 1 0))
                  (ws-food ws)))]
    [(key=? ke "left")
     (if (dir-not-set? (ws-worm ws))
         (make-ws (set-dir (ws-worm ws) (make-dir -1 0))
                  (ws-food ws))
         (make-ws (set-head-dir (ws-worm ws) (make-dir -1 0))
                  (ws-food ws)))]
    [else ws]))

(check-expect (ke-handler (make-ws TEST-WORM (make-posn 5 115)) "up")
              (make-ws
               (list (make-worm (worm-pos (first TEST-WORM)) (make-dir 0 -1))
                     (make-worm (worm-pos (second TEST-WORM)) (make-dir 0 -1))
                     (make-worm (worm-pos (third TEST-WORM)) (make-dir 0 -1))
                     (make-worm (worm-pos (fourth TEST-WORM)) (make-dir 0 -1))
                     (make-worm (worm-pos (fifth TEST-WORM)) (make-dir 0 -1)))
              (make-posn 5 115)))
(check-expect (ke-handler (make-ws TEST-WORM (make-posn 5 115)) "down")
              (make-ws
               (list (make-worm (worm-pos (first TEST-WORM)) (make-dir 0 1))
                     (make-worm (worm-pos (second TEST-WORM)) (make-dir 0 1))
                     (make-worm (worm-pos (third TEST-WORM)) (make-dir 0 1))
                     (make-worm (worm-pos (fourth TEST-WORM)) (make-dir 0 1))
                     (make-worm (worm-pos (fifth TEST-WORM)) (make-dir 0 1)))
               (make-posn 5 115)))
(check-expect (ke-handler (make-ws TEST-WORM (make-posn 5 115)) "right")
              (make-ws
               (list (make-worm (worm-pos (first TEST-WORM)) (make-dir 1 0))
                     (make-worm (worm-pos (second TEST-WORM)) (make-dir 1 0))
                     (make-worm (worm-pos (third TEST-WORM)) (make-dir 1 0))
                     (make-worm (worm-pos (fourth TEST-WORM)) (make-dir 1 0))
                     (make-worm (worm-pos (fifth TEST-WORM)) (make-dir 1 0)))
               (make-posn 5 115)))
(check-expect (ke-handler (make-ws TEST-WORM (make-posn 5 115)) "left")
              (make-ws
               (list (make-worm (worm-pos (first TEST-WORM)) (make-dir -1 0))
                     (make-worm (worm-pos (second TEST-WORM)) (make-dir -1 0))
                     (make-worm (worm-pos (third TEST-WORM)) (make-dir -1 0))
                     (make-worm (worm-pos (fourth TEST-WORM)) (make-dir -1 0))
                     (make-worm (worm-pos (fifth TEST-WORM)) (make-dir -1 0)))
               (make-posn 5 115)))

(check-expect (ke-handler (make-ws TEST-WORM-2 (make-posn 5 115)) "up")
              (make-ws
               (list (make-worm (worm-pos (first TEST-WORM-2)) (make-dir 0 -1))
                     (second TEST-WORM-2)
                     (third TEST-WORM-2)
                     (fourth TEST-WORM-2)
                     (fifth TEST-WORM-2))
               (make-posn 5 115)))
(check-expect (ke-handler (make-ws TEST-WORM-2 (make-posn 5 115)) "down")
              (make-ws
               (list (make-worm (worm-pos (first TEST-WORM-2)) (make-dir 0 1))
                     (second TEST-WORM-2)
                     (third TEST-WORM-2)
                     (fourth TEST-WORM-2)
                     (fifth TEST-WORM-2))
               (make-posn 5 115)))
(check-expect (ke-handler (make-ws TEST-WORM-2 (make-posn 5 115)) "right")
              (make-ws
               (list (make-worm (worm-pos (first TEST-WORM-2)) (make-dir 1 0))
                     (second TEST-WORM-2)
                     (third TEST-WORM-2)
                     (fourth TEST-WORM-2)
                     (fifth TEST-WORM-2))
               (make-posn 5 115)))
(check-expect (ke-handler (make-ws TEST-WORM-2 (make-posn 5 115)) "left")
              (make-ws
               (list (make-worm (worm-pos (first TEST-WORM-2)) (make-dir -1 0))
                     (second TEST-WORM-2)
                     (third TEST-WORM-2)
                     (fourth TEST-WORM-2)
                     (fifth TEST-WORM-2))
               (make-posn 5 115)))

(check-expect (ke-handler (make-ws TEST-WORM (make-posn 5 115)) " ")
              (make-ws TEST-WORM (make-posn 5 115)))

; NOTE -------------------------------------------------------------------------

; The move-worm function does not follow the design recipe, as the logic does
; not mirror the data definition but different states / values the data can take
; on. This is necessary because when the game is started the worm is not
; moving, i.e. the direction vectors are (make-dir 0 0). Would the function not
; test for this state, the worm would shrink into one segment and only grow
; back to full size when a arrow button has been pressed.
; Alternatively one could initialize the game with all direction vectors set
; to a certain value. In this case this logic testing would not be necessary.

; END NOTE ---------------------------------------------------------------------

; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

; One way to interpret “eating” is to say that the head moves where the food
; used to be located and the tail grows by one segment, inserted where the head
; used to be. Why is this interpretation easy to design as a function?

; A slightly different solution is employed here. When food has been eaten by
; the worm, a new worm segment is added one "spot" after the spot of the food
; in the direction the head was moving while the rest of the worm is standing
; still. Also, this is not implemented as a seperate function, but within the
; move-worm function.

; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

; WorldState -> WorldState
; Change the position of the worm
; every clock tick according to its
; directional vector.
; When the worm has eaten food, create
; new food and make the worm one segment
; longer.
(define (move-worm ws)
  (cond
    [(dir-not-set? (ws-worm ws)) ws]
    [(food-eaten? ws)
     (make-ws
      (cons
       (make-worm
        (make-posn (+ (worm-x (first (ws-worm ws)))
                      (* SPEED (worm-x-dir (first (ws-worm ws)))))
                   (+ (worm-y (first (ws-worm ws)))
                      (* SPEED (worm-y-dir (first (ws-worm ws))))))
        (worm-dir (first (ws-worm ws))))
       (ws-worm ws))
      (food-create (ws-food ws)))]
    [else
     (make-ws
      (delete-last
       (cons
        (make-worm (make-posn (+ (worm-x (first (ws-worm ws)))
                                 (* SPEED (worm-x-dir (first (ws-worm ws)))))
                              (+ (worm-y (first (ws-worm ws)))
                                 (* SPEED (worm-y-dir (first (ws-worm ws))))))
                   (worm-dir (first (ws-worm ws))))
        (ws-worm ws)))
      (ws-food ws))]))

(check-expect (move-worm (make-ws TEST-WORM (make-posn 5 115)))
              (make-ws TEST-WORM (make-posn 5 115)))
(check-satisfied (move-worm (make-ws TEST-WORM-2
                                    (first (get-worm-posns TEST-WORM))))
                 longer-test-worm2-and-new-food?)
(check-expect (move-worm
               (make-ws
                (list (make-worm (make-posn 50 50) (make-dir 1 0))
                      (make-worm (make-posn 50 40) (make-dir 0 1))
                      (make-worm (make-posn 40 40) (make-dir 1 0)))
                (make-posn 5 115)))
               (make-ws
                (list (make-worm (make-posn (+ 50 SPEED) 50) (make-dir 1 0))
                      (make-worm (make-posn 50 (+ 40 SPEED)) (make-dir 1 0))
                      (make-worm (make-posn (+ 40 SPEED) 40) (make-dir 0 1)))
               (make-posn 5 115)))

; WorldState -> Boolean
; Has the worm run into the walls of the
; world or itself?
(define (game-over? ws)
  (or (run-into-walls? (ws-worm ws))
      (run-into-self? (ws-worm ws))))

(check-expect (game-over? (make-ws TEST-WORM (make-posn 75 95))) 
              #false)
(check-expect (game-over? (make-ws (list (make-worm (make-posn -10 50)
                                                    (make-dir -1 0)))
                                   (make-posn 75 95)))
              #true)
(check-expect (game-over? (make-ws (cons (first TEST-WORM)
                                         TEST-WORM)
                                   (make-posn 75 95)))
              #true)

; WorldState -> Image
; Render the final state of the world
; as an image after the game has ended.
(define (render-final ws)
  (place-image/align (cond
                       [(run-into-walls? (ws-worm ws))
                        GAME-OVER-WALLS]
                       [(run-into-self? (ws-worm ws))
                        GAME-OVER-SELF])
                     (* 1 RADIUS)
                     (- (image-height BACKGROUND)
                        (* 1/2 GAME-OVER-HEIGHT)
                        (* 0 RADIUS))
                     "left"
                     "middle"
                     (render ws)))

(check-expect (render-final (make-ws
                             (list
                              (make-worm
                               (make-posn (+ (image-width BACKGROUND) 10) 50)
                               (make-dir -1 0)))
                             (make-posn 135 55)))
              (place-image/align GAME-OVER-WALLS
                                 (* 1 RADIUS)
                                 (- (image-height BACKGROUND)
                                    (* 1/2 GAME-OVER-HEIGHT)
                                    (* 0 RADIUS))
                                 "left"
                                 "middle"
                                 (render
                                  (make-ws
                                   (list
                                    (make-worm
                                     (make-posn (+ (image-width BACKGROUND) 10)
                                                50)
                                     (make-dir -1 0)))
                                   (make-posn 135 55)))))

(check-expect (render-final
               (make-ws
                (list (make-worm (make-posn 105 135) (make-dir 0 -1))
                      (make-worm (make-posn 105 145) (make-dir 0 -1))
                      (make-worm (make-posn 105 155) (make-dir 0 -1))
                      (make-worm (make-posn 105 145) (make-dir 0 1))
                      (make-worm (make-posn 105 135) (make-dir 0 1)))
                (make-posn 135 55)))
              (place-image/align
               GAME-OVER-SELF
               (* 1 RADIUS)
               (- (image-height BACKGROUND)
                  (* 1/2 GAME-OVER-HEIGHT)
                  (* 0 RADIUS))
               "left"
               "middle"
               (render
                (make-ws
                 (list
                  (make-worm (make-posn 105 135) (make-dir 0 -1))
                  (make-worm (make-posn 105 145) (make-dir 0 -1))
                  (make-worm (make-posn 105 155) (make-dir 0 -1))
                  (make-worm (make-posn 105 145) (make-dir 0 1))
                  (make-worm (make-posn 105 135) (make-dir 0 1)))
                 (make-posn 135 55)))))

; Number -> NE-Worm
; Create a worm with n segments,
; limit the number of segments to 10.
(define (create-worm n)
  (cond
    [(or (not (integer? n))
         (< n 1)
         (> n 10))
     (error NUMBER-ERROR)]
    [else (make-worm-segments n)]))

(check-error (create-worm 0)
             NUMBER-ERROR)
(check-error (create-worm 5.5)
             NUMBER-ERROR)
(check-error (create-worm 11)
             NUMBER-ERROR)
(check-expect (create-worm 5)
              TEST-WORM)

; Posn -> Posn 
; Create a random position for the food.
(define (food-create p)
  (food-check-create
     p (make-posn (random-diameter MAX-X) (random-diameter MAX-Y))))

(check-satisfied (food-create (make-posn 1 1))
                 divisible-by-RADIUS-and-within-canvas?)

; Worm Image -> Image
; Render the worm and place it in the image img.
(define (render-worm worm img)
  (cond
    [(empty? (rest worm))
     (place-image WORM
                  (worm-x (first worm))
                  (worm-y (first worm))
                  img)]
    [else
     (place-image WORM
                  (worm-x (first worm))
                  (worm-y (first worm))
                  (render-worm (rest worm) img))]))

(check-expect (render-worm TEST-WORM BACKGROUND)
              (place-image
               WORM
               (worm-x (first TEST-WORM))
               (worm-y (first TEST-WORM))
               (place-image
                WORM
                (worm-x (second TEST-WORM))
                (worm-y (second TEST-WORM))
                (place-image
                 WORM
                 (worm-x (third TEST-WORM))
                 (worm-y (third TEST-WORM))
                 (place-image
                  WORM
                  (worm-x (fourth TEST-WORM))
                  (worm-y (fourth TEST-WORM))
                  (place-image
                   WORM
                   (worm-x (fifth TEST-WORM))
                   (worm-y (fifth TEST-WORM))
                   BACKGROUND))))))

; NE-Worm -> Boolean
; Is any directional vector of any
; of the worm segments equal to (make-dir 0 0)?
(define (dir-not-set? new)
  (cond
    [(empty? (rest new))
     (and (zero? (worm-x-dir (first new)))
          (zero? (worm-y-dir (first new))))]
    [else
     (or (and (zero? (worm-x-dir (first new)))
              (zero? (worm-y-dir (first new))))
         (dir-not-set? (rest new)))]))

(check-expect (dir-not-set? (list
                             (make-worm (make-posn 10 10) (make-dir -1 0))
                             (make-worm (make-posn 20 10) (make-dir -1 0))
                             (make-worm (make-posn 30 10) (make-dir -1 0))))
              #false)
(check-expect (dir-not-set? (list
                             (make-worm (make-posn 10 10) (make-dir -1 0))
                             (make-worm (make-posn 20 10) (make-dir -1 0))
                             (make-worm (make-posn 30 10) (make-dir 0 0))))
              #true)
(check-expect (dir-not-set? TEST-WORM)
              #true)

; NE-Worm -> NE-Worm
; Set all directional vectors of news
; to dirvec.
(define (set-dir new dirvec)
  (cond
    [(empty? (rest new))
     (list (make-worm (worm-pos (first new)) dirvec))]
    [else
     (append
      (list
       (make-worm (worm-pos (first new))
                  dirvec))
       (set-dir (rest new) dirvec))]))

(check-expect (set-dir TEST-WORM (make-dir 0 -1))
              (list (make-worm (worm-pos (first TEST-WORM)) (make-dir 0 -1))
                    (make-worm (worm-pos (second TEST-WORM)) (make-dir 0 -1))
                    (make-worm (worm-pos (third TEST-WORM)) (make-dir 0 -1))
                    (make-worm (worm-pos (fourth TEST-WORM)) (make-dir 0 -1))
                    (make-worm (worm-pos (fifth TEST-WORM)) (make-dir 0 -1))))

; NE-Worm -> NE-WorldState
; Set the directional vector of the
; head of the worm to dirvec.
(define (set-head-dir new dirvec)
  (cons (make-worm (worm-pos (first new)) dirvec)
        (rest new)))

(check-expect (set-head-dir TEST-WORM (make-dir 0 -1))
              (list (make-worm (worm-pos (first TEST-WORM)) (make-dir 0 -1))
                    (second TEST-WORM)
                    (third TEST-WORM)
                    (fourth TEST-WORM)
                    (fifth TEST-WORM)))

; WorldState -> Boolean
; Has the worm eaten the food?
(define (food-eaten? ws)
  (equal? (get-head-posn (ws-worm ws))
          (ws-food ws)))

(check-expect (food-eaten? (make-ws TEST-WORM
                                    (make-posn (+ (worm-x (first TEST-WORM)) 10)
                                               (- (* 1/2 HEIGHT) RADIUS))))
              #false)
(check-expect (food-eaten? (make-ws TEST-WORM
                                    (first (get-worm-posns TEST-WORM))))
              #true)

; NE-Worm -> NE-Worm or Empty
; Delete the last worm segment (tail).
(define (delete-last new)
  (cond
    [(empty? (rest new)) '()]
    [else
     (cons (first new) (delete-last (rest new)))]))

(check-expect (delete-last (list (make-worm (make-posn 50 50) (make-dir 1 0))))
              '())
(check-expect (delete-last (list (make-worm (make-posn 50 50) (make-dir 1 0))
                                 (make-worm (make-posn 50 40) (make-dir 0 1))
                                 (make-worm (make-posn 40 40) (make-dir 1 0))))
              (list (make-worm (make-posn 50 50) (make-dir 1 0))
                    (make-worm (make-posn 50 40) (make-dir 0 1))))

; NE-Worm -> Boolean
; Has the worm run into the walls of the world?
(define (run-into-walls? new)
  (cond
    ; position is outside the canvas in x-direction
    [(or (< (- (worm-x (first new)) RADIUS) 0)
         (> (+ (worm-x (first new)) RADIUS) (image-width BACKGROUND)))
     #true]
    ; position is outside the canvas is y-direction
    [(or (< (- (worm-y (first new)) RADIUS) 0)
         (> (+ (worm-y (first new)) RADIUS) (image-height BACKGROUND)))
     #true]
    [else #false]))

(check-expect (run-into-walls? TEST-WORM)
              #false)
(check-expect (run-into-walls? (list (make-worm (make-posn -10 50)
                                                (make-dir -1 0))))
              #true)
(check-expect (run-into-walls? (list
                                (make-worm
                                 (make-posn (+ (image-width BACKGROUND) 10) 50)
                                 (make-dir -1 0))))
              #true)
(check-expect (run-into-walls? (list (make-worm (make-posn 50 -10)
                                                (make-dir -1 0))))
              #true)
(check-expect (run-into-walls? (list
                                (make-worm
                                 (make-posn 50
                                            (+ (image-height BACKGROUND) 10))
                                 (make-dir -1 0))))
              #true)

; NE-Worm -> Boolean
; Has the worm run into itself?
(define (run-into-self? new)
  (cond
    [(empty? (rest new)) #false]
    [else
     (member? (worm-pos (first new))
              (get-worm-posns (rest new)))]))

(check-expect (run-into-self? TEST-WORM)
              #false)
(check-expect (run-into-self? (cons (first TEST-WORM)
                                    TEST-WORM))
              #true)
(check-expect (run-into-self?
               (list (make-worm (make-posn 105 135) (make-dir 0 -1))
                     (make-worm (make-posn 105 145) (make-dir 0 -1))
                     (make-worm (make-posn 105 155) (make-dir 0 -1))
                     (make-worm (make-posn 105 145) (make-dir 0 1))
                     (make-worm (make-posn 105 135) (make-dir 0 1))))
              #true)

; Number -> NE-Worm
; Create a worm with n segments.
(define (make-worm-segments n)
  (cond
    [(<= n 0) '()]
    [(> n 0)
     (cons
      (make-worm (make-posn (+ (* 1/2 WIDTH) RADIUS)
                            (+ (* 1/2 HEIGHT) (* RADIUS (- (* 2 n) 11))))
                 (make-dir 0 0))
      (make-worm-segments (sub1 n)))]))

(check-expect (make-worm-segments 0)
              '())
(check-expect (make-worm-segments 5)
              TEST-WORM)

; NE-Worm -> Posn
; Return the current position of
; the worm's head.
(define (get-head-posn new)
  (first (get-worm-posns new)))

(check-expect (get-head-posn TEST-WORM)
              (worm-pos (first TEST-WORM)))

; NE-Worm -> List-of-Posns
; Return a list of the positions of the worm segments.
(define (get-worm-posns new)
  (cond
    [(empty? (rest new))
     (list (worm-pos (first new)))]
    [else
     (append
      (list (worm-pos (first new)))
      (get-worm-posns (rest new)))]))

(check-expect (get-worm-posns (list
                               (make-worm (make-posn 10 10)
                                          (make-dir 1 0))))
              (list (make-posn 10 10)))

; WormSegment -> Number
; Return the current x-position
; of the worm segment news.
(define (worm-x ws)
  (posn-x (worm-pos ws)))

(check-expect (worm-x (make-worm (make-posn 75 100) (make-dir 1 0)))
              75)

; WormSegment -> Number
; Return the current y-position
; of the worm segment news.
(define (worm-y ws)
  (posn-y (worm-pos ws)))

(check-expect (worm-y (make-worm (make-posn 75 100) (make-posn 1 0)))
              100)

; WormSegment -> Number
; Return the current x-direction
; of the worm segment news.
(define (worm-x-dir ws)
  (dir-x (worm-dir ws)))

(check-expect (worm-x-dir (make-worm (make-posn 75 100) (make-dir 1 0)))
              1)

; WormSegment -> Number
; Return the current y-direction
; of the worm segment news.
(define (worm-y-dir ws)
  (dir-y (worm-dir ws)))

(check-expect (worm-y-dir (make-worm (make-posn 75 100) (make-dir 1 0)))
              0)

; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

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

(check-satisfied (random-diameter 20)
                 divisible-by-RADIUS-but-not-2RADIUS-and-within-canvas?)

; N -> N
; Return a random number x that is divisible by (* 2 RADIUS)
; and (* 2 RADIUS) <= x <= (* (* 2 RADIUS) n).
(define (random-2r n)
  (* (* RADIUS 2) (+ (random n) 1)))

(check-satisfied (random-2r 20)
                 divisible-by-RADIUS-but-not-2RADIUS-and-within-canvas?)

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

; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

; WordState -> Boolean
; for testing only
(define (longer-test-worm2-and-new-food? ws)
  (and (equal? (ws-worm ws)
               (cons
                (make-worm
                 (make-posn (+ (worm-x (first TEST-WORM-2))
                               (* SPEED (worm-x-dir
                                         (first TEST-WORM-2))))
                            (+ (worm-y (first TEST-WORM-2))
                               (* SPEED (worm-y-dir
                                         (first TEST-WORM-2)))))
                 (worm-dir (first TEST-WORM-2)))
                TEST-WORM-2))
       (divisible-by-RADIUS-and-within-canvas? (ws-food ws))))
