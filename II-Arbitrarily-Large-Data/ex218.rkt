;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex218) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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
;     (make-ws Posn DirVec)
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

; A NE-WorldState (non-empty world state) is one of:
; – (cons WormSegment '())
; – (cons WormSegment NE-WorldState)
; Interpretation:
;     The segment(s) of the worm,
;     head first, tail last.

; CONSTANTS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define RADIUS 5)                          ; radius of one segment of the worm
(define WIDTH (min 1000 (* 40 RADIUS)))    ; width of the canvas
(define HEIGHT (min 1250 (* 52 RADIUS)))   ; height of the canvas
(define SPEED (* 2 RADIUS))                ; the speed of the worm

(define WORM (circle RADIUS "solid" "red"))
(define BACKGROUND (empty-scene WIDTH HEIGHT "Midnight Blue"))

(define NUMBER-ERROR (string-append "The number must be an integer between "
                                    "1 and 10 inclusive."))

(define GAME-OVER-WALLS (text "worm hit border" (* 4 RADIUS) "red"))
(define GAME-OVER-SELF (text "worm ran into itself" (* 4 RADIUS) "red"))
(define GAME-OVER-HEIGHT (max (image-height GAME-OVER-WALLS)
                              (image-height GAME-OVER-SELF)))

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
                   (make-dir 0 -1))
        (make-worm (make-posn (+ (* 1/2 WIDTH) RADIUS)
                              (- (* 1/2 HEIGHT) (* 3 RADIUS)))
                   (make-dir 0 -1))
        (make-worm (make-posn (+ (* 1/2 WIDTH) RADIUS)
                              (- (* 1/2 HEIGHT) (* 5 RADIUS)))
                   (make-dir 0 -1))
        (make-worm (make-posn (+ (* 1/2 WIDTH) RADIUS)
                              (- (* 1/2 HEIGHT) (* 7 RADIUS)))
                   (make-dir 0 -1))
        (make-worm (make-posn (+ (* 1/2 WIDTH) RADIUS)
                              (- (* 1/2 HEIGHT) (* 9 RADIUS)))
                   (make-dir 0 -1))))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Number -> NE-WorldState
; Start the program here.
; Pass it the time between clock ticks in seconds
; and the number of worm segments.
(define (worm-main s n)
  (big-bang (create-worm n)
    [on-draw render]
    [on-key ke-handler]
    [on-tick move-worm s]
    [stop-when game-over? render-final]))

; Number -> NE-WorldState
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

; NE-WorldState -> Image
; Render the current state of the
; world as an image.
(define (render news)
  (cond
    [(empty? (rest news))
     (place-image WORM
                  (worm-x (first news))
                  (worm-y (first news))
                  BACKGROUND)]
    [else
     (place-image WORM
                  (worm-x (first news))
                  (worm-y (first news))
                  (render (rest news)))]))

(check-expect (render (list (make-worm (make-posn 75 100) (make-dir 1 0))))
              (place-image WORM
                           75
                           100
                           BACKGROUND))
(check-expect (render
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
                 (make-dir 0 0))))
              (place-image WORM
                           (+ (* 1/2 WIDTH) RADIUS)
                           (- (* 1/2 HEIGHT) (* 5 RADIUS))
                           (place-image WORM
                                        (+ (* 1/2 WIDTH) RADIUS)
                                        (- (* 1/2 HEIGHT) (* 3 RADIUS))
                                        (place-image WORM
                                                     (+ (* 1/2 WIDTH) RADIUS)
                                                     (- (* 1/2 HEIGHT) RADIUS)
                                                     BACKGROUND))))

; NE-WorldState -> Image
; Render the final state of the world
; as an image after the games has ended.
(define (render-final news)
  (place-image/align (cond
                       [(run-into-walls? news)
                        GAME-OVER-WALLS]
                       [(run-into-self? news)
                        GAME-OVER-SELF])
                     (* 1 RADIUS)
                     (- (image-height BACKGROUND)
                        (* 1/2 GAME-OVER-HEIGHT)
                        (* 0 RADIUS))
                     "left"
                     "middle"
                     (render news)))

(check-expect (render-final (list
                             (make-worm
                              (make-posn (+ (image-width BACKGROUND) 10) 50)
                              (make-dir -1 0))))
              (place-image/align GAME-OVER-WALLS
                                 (* 1 RADIUS)
                                 (- (image-height BACKGROUND)
                                    (* 1/2 GAME-OVER-HEIGHT)
                                    (* 0 RADIUS))
                                 "left"
                                 "middle"
                                 (render
                                  (list
                                   (make-worm
                                    (make-posn (+ (image-width BACKGROUND) 10)
                                               50)
                                    (make-dir -1 0))))))

(check-expect (render-final
               (list (make-worm (make-posn 105 135) (make-dir 0 -1))
                     (make-worm (make-posn 105 145) (make-dir 0 -1))
                     (make-worm (make-posn 105 155) (make-dir 0 -1))
                     (make-worm (make-posn 105 145) (make-dir 0 1))
                     (make-worm (make-posn 105 135) (make-dir 0 1))))
              (place-image/align
               GAME-OVER-SELF
               (* 1 RADIUS)
               (- (image-height BACKGROUND)
                  (* 1/2 GAME-OVER-HEIGHT)
                  (* 0 RADIUS))
               "left"
               "middle"
               (render
                (list
                 (make-worm (make-posn 105 135) (make-dir 0 -1))
                 (make-worm (make-posn 105 145) (make-dir 0 -1))
                 (make-worm (make-posn 105 155) (make-dir 0 -1))
                 (make-worm (make-posn 105 145) (make-dir 0 1))
                 (make-worm (make-posn 105 135) (make-dir 0 1))))))

; NE-WorldState KeyEvent -> NE-WorldState
; Change the direction of the worm according
; to the arrow key pressed.
(define (ke-handler news ke)
  (cond
    [(key=? ke "up")
     (if (dir-not-set? news)
         (set-dir news (make-dir 0 -1))
         (set-head-dir news (make-dir 0 -1)))]
    [(key=? ke "down")
     (if (dir-not-set? news)
         (set-dir news (make-dir 0 1))
         (set-head-dir news (make-dir 0 1)))]
    [(key=? ke "right")
     (if (dir-not-set? news)
         (set-dir news (make-dir 1 0))
         (set-head-dir news (make-dir 1 0)))]
    [(key=? ke "left")
     (if (dir-not-set? news)
         (set-dir news (make-dir -1 0))
         (set-head-dir news (make-dir -1 0)))]
    [else news]))

(check-expect (ke-handler TEST-WORM "up")
              (list (make-worm (worm-pos (first TEST-WORM)) (make-dir 0 -1))
                    (make-worm (worm-pos (second TEST-WORM)) (make-dir 0 -1))
                    (make-worm (worm-pos (third TEST-WORM)) (make-dir 0 -1))
                    (make-worm (worm-pos (fourth TEST-WORM)) (make-dir 0 -1))
                    (make-worm (worm-pos (fifth TEST-WORM)) (make-dir 0 -1))))
(check-expect (ke-handler TEST-WORM "down")
              (list (make-worm (worm-pos (first TEST-WORM)) (make-dir 0 1))
                    (make-worm (worm-pos (second TEST-WORM)) (make-dir 0 1))
                    (make-worm (worm-pos (third TEST-WORM)) (make-dir 0 1))
                    (make-worm (worm-pos (fourth TEST-WORM)) (make-dir 0 1))
                    (make-worm (worm-pos (fifth TEST-WORM)) (make-dir 0 1))))
(check-expect (ke-handler TEST-WORM "right")
              (list (make-worm (worm-pos (first TEST-WORM)) (make-dir 1 0))
                    (make-worm (worm-pos (second TEST-WORM)) (make-dir 1 0))
                    (make-worm (worm-pos (third TEST-WORM)) (make-dir 1 0))
                    (make-worm (worm-pos (fourth TEST-WORM)) (make-dir 1 0))
                    (make-worm (worm-pos (fifth TEST-WORM)) (make-dir 1 0))))
(check-expect (ke-handler TEST-WORM "left")
              (list (make-worm (worm-pos (first TEST-WORM)) (make-dir -1 0))
                    (make-worm (worm-pos (second TEST-WORM)) (make-dir -1 0))
                    (make-worm (worm-pos (third TEST-WORM)) (make-dir -1 0))
                    (make-worm (worm-pos (fourth TEST-WORM)) (make-dir -1 0))
                    (make-worm (worm-pos (fifth TEST-WORM)) (make-dir -1 0))))

(check-expect (ke-handler TEST-WORM-2 "up")
              (list (make-worm (worm-pos (first TEST-WORM-2)) (make-dir 0 -1))
                    (second TEST-WORM-2)
                    (third TEST-WORM-2)
                    (fourth TEST-WORM-2)
                    (fifth TEST-WORM-2)))
(check-expect (ke-handler TEST-WORM-2 "down")
              (list (make-worm (worm-pos (first TEST-WORM-2)) (make-dir 0 1))
                    (second TEST-WORM-2)
                    (third TEST-WORM-2)
                    (fourth TEST-WORM-2)
                    (fifth TEST-WORM-2)))
(check-expect (ke-handler TEST-WORM-2 "right")
              (list (make-worm (worm-pos (first TEST-WORM-2)) (make-dir 1 0))
                    (second TEST-WORM-2)
                    (third TEST-WORM-2)
                    (fourth TEST-WORM-2)
                    (fifth TEST-WORM-2)))
(check-expect (ke-handler TEST-WORM-2 "left")
              (list (make-worm (worm-pos (first TEST-WORM-2)) (make-dir -1 0))
                    (second TEST-WORM-2)
                    (third TEST-WORM-2)
                    (fourth TEST-WORM-2)
                    (fifth TEST-WORM-2)))

(check-expect (ke-handler TEST-WORM " ")
              TEST-WORM)

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
; NE-WorldState -> NE-WorldState
; Change the position of the worm
; every clock tick according to its
; directional vector.
(define (move-worm news)
  (cond
    [(dir-not-set? news) news]
    [else
     (delete-last
      (cons
       (make-worm (make-posn (+ (worm-x (first news))
                                (* SPEED (worm-x-dir (first news))))
                             (+ (worm-y (first news))
                                (* SPEED (worm-y-dir (first news)))))
                  (worm-dir (first news)))
       news))]))

(check-expect (move-worm TEST-WORM)
              TEST-WORM)
(check-expect (move-worm (list (make-worm (make-posn 50 50) (make-dir 1 0))
                               (make-worm (make-posn 50 40) (make-dir 0 1))
                               (make-worm (make-posn 40 40) (make-dir 1 0))))
              (list (make-worm (make-posn (+ 50 SPEED) 50) (make-dir 1 0))
                    (make-worm (make-posn 50 (+ 40 SPEED)) (make-dir 1 0))
                    (make-worm (make-posn (+ 40 SPEED) 40) (make-dir 0 1))))

; NE-WorldState -> Boolean
; Has the worm run into the walls of the
; world or itself?
(define (game-over? news)
  (or (run-into-walls? news)
      (run-into-self? news)))

(check-expect (game-over? TEST-WORM)
              #false)
(check-expect (game-over? (list (make-worm (make-posn -10 50)
                                           (make-dir -1 0))))
              #true)
(check-expect (game-over? (cons (first TEST-WORM)
                                TEST-WORM))
              #true)

; Number -> NE-WorldState
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

; NE-WorldState -> NE-WorldState
; Set the directional vector of the
; head of the worm to dirvec.
(define (set-head-dir news dirvec)
  (cons (make-worm (worm-pos (first news)) dirvec)
        (rest news)))

(check-expect (set-head-dir TEST-WORM (make-dir 0 -1))
              (list (make-worm (worm-pos (first TEST-WORM)) (make-dir 0 -1))
                    (second TEST-WORM)
                    (third TEST-WORM)
                    (fourth TEST-WORM)
                    (fifth TEST-WORM)))

; NE-WorldState -> NE-WorldState
; Set all directional vectors of news
; to dirvec.
(define (set-dir news dirvec)
  (cond
    [(empty? (rest news))
     (list (make-worm (worm-pos (first news)) dirvec))]
    [else
     (append
      (list
       (make-worm (worm-pos (first news))
                  dirvec))
       (set-dir (rest news) dirvec))]))

(check-expect (set-dir TEST-WORM (make-dir 0 -1))
              (list (make-worm (worm-pos (first TEST-WORM)) (make-dir 0 -1))
                    (make-worm (worm-pos (second TEST-WORM)) (make-dir 0 -1))
                    (make-worm (worm-pos (third TEST-WORM)) (make-dir 0 -1))
                    (make-worm (worm-pos (fourth TEST-WORM)) (make-dir 0 -1))
                    (make-worm (worm-pos (fifth TEST-WORM)) (make-dir 0 -1))))

; NE-WorldState -> Boolean
; Is any directional vector of any
; of the worm segments equal to (make-dir 0 0)?
(define (dir-not-set? news)
  (cond
    [(empty? (rest news))
     (and (zero? (worm-x-dir (first news)))
          (zero? (worm-y-dir (first news))))]
    [else
     (or (and (zero? (worm-x-dir (first news)))
              (zero? (worm-y-dir (first news))))
         (dir-not-set? (rest news)))]))

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

; NE-WorldState -> NE-WorldState or empty
; Delete the last worm segment (tail).
(define (delete-last news)
  (cond
    [(empty? (rest news)) '()]
    [else
     (cons (first news) (delete-last (rest news)))]))

(check-expect (delete-last (list (make-worm (make-posn 50 50) (make-dir 1 0))))
              '())
(check-expect (delete-last (list (make-worm (make-posn 50 50) (make-dir 1 0))
                                 (make-worm (make-posn 50 40) (make-dir 0 1))
                                 (make-worm (make-posn 40 40) (make-dir 1 0))))
              (list (make-worm (make-posn 50 50) (make-dir 1 0))
                    (make-worm (make-posn 50 40) (make-dir 0 1))))

; NE-WorldState -> Boolean
; Has the worm run into the walls of the world?
(define (run-into-walls? news)
  (cond
    ; position is outside the canvas in x-direction
    [(or (< (- (worm-x (first news)) RADIUS) 0)
         (> (+ (worm-x (first news)) RADIUS) (image-width BACKGROUND)))
     #true]
    ; position is outside the canvas is y-direction
    [(or (< (- (worm-y (first news)) RADIUS) 0)
         (> (+ (worm-y (first news)) RADIUS) (image-height BACKGROUND)))
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

; NE-WorldState -> Boolean
; Has the worm run into itself?
(define (run-into-self? news)
  (member? (worm-pos (first news))
           (get-worm-posns (rest news))))

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

; NE-WorldState -> List-of-Posns
; Return a list of the positions of the worm segments.
(define (get-worm-posns news)
  (cond
    [(empty? (rest news))
     (list (worm-pos (first news)))]
    [else
     (append
      (list (worm-pos (first news)))
      (get-worm-posns (rest news)))]))

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
