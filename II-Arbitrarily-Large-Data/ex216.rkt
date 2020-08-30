;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex216) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define-struct ws [pos dir])
; A WorldState is a structure:
;     (make-ws Posn Posn)
; Interpretation:
;     The current position of the worm
;     on the canvas and its directional vector.
;     The directional vector should only contain
;     the integers -1, 0 and 1.

; CONSTANTS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define RADIUS 5)                          ; radius of one segment of the worm
(define WIDTH (min 1000 (* 40 RADIUS)))    ; width of the canvas
(define HEIGHT (min 1250 (* 52 RADIUS)))   ; height of the canvas
(define SPEED (* 2 RADIUS))                ; the speed of the worm

(define WORM (circle RADIUS "solid" "red"))
(define BACKGROUND (empty-scene WIDTH HEIGHT "Midnight Blue"))
(define GAME-OVER (text "worm hit border" (* 4 RADIUS) "red"))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Number -> WorldState
; Start the program here.
; Pass it the time between
; clock ticks in seconds.
(define (worm-main s)
  (big-bang (make-ws (make-posn (- (* 1/2 WIDTH) RADIUS)
                                (- (* 1/2 HEIGHT) RADIUS))
                     (make-posn 0 0))
    [on-draw render]
    [on-key ke-handler]
    [on-tick tock s]
    [stop-when hit-wall? render-final]))

; WorldState -> Image
; Render the current state of the
; world as an image.
(define (render ws)
  (place-image WORM
               (worm-x ws)
               (worm-y ws)
               BACKGROUND))

(check-expect (render (make-ws (make-posn 75 100) (make-posn 1 0)))
              (place-image WORM
                           75
                           100
                           BACKGROUND))

; WorldState KeyEvent -> WorldState
; Change the direction of the worm according
; to the arrow key pressed.
(define (ke-handler ws ke)
  (cond
    [(key=? ke "up") (make-ws (ws-pos ws) (make-posn 0 -1))]
    [(key=? ke "down") (make-ws (ws-pos ws) (make-posn 0 1))]
    [(key=? ke "right") (make-ws (ws-pos ws) (make-posn 1 0))]
    [(key=? ke "left") (make-ws (ws-pos ws) (make-posn -1 0))]
    [else ws]))

(check-expect (ke-handler (make-ws (make-posn 50 70) (make-posn 0 1)) "up")
              (make-ws (make-posn 50 70) (make-posn 0 -1)))
(check-expect (ke-handler (make-ws (make-posn 50 70) (make-posn 0 1)) "down")
              (make-ws (make-posn 50 70) (make-posn 0 1)))
(check-expect (ke-handler (make-ws (make-posn 50 70) (make-posn 0 1)) "left")
              (make-ws (make-posn 50 70) (make-posn -1 0)))
(check-expect (ke-handler (make-ws (make-posn 50 70) (make-posn -1 0)) "right")
              (make-ws (make-posn 50 70) (make-posn 1 0)))

(check-expect (ke-handler (make-ws (make-posn 50 70) (make-posn 0 1)) " ")
              (make-ws (make-posn 50 70) (make-posn 0 1)))

; WorldState -> WorldState
; Change the position of the worm
; every clock tick according to its
; speed and directional vector.
(define (tock ws)
  (make-ws (make-posn (+ (worm-x ws)
                         (* (worm-x-dir ws) SPEED))
                      (+ (worm-y ws)
                         (* (worm-y-dir ws) SPEED)))
           (ws-dir ws)))

(check-expect (tock (make-ws (make-posn 50 75) (make-posn 1 0)))
              (make-ws (make-posn (+ 50 (* 1 SPEED)) 75)
                       (make-posn 1 0)))
(check-expect (tock (make-ws (make-posn 50 75) (make-posn 0 -1)))
              (make-ws (make-posn 50 (+ 75 (* -1 SPEED)))
                       (make-posn 0 -1)))

; WorldState -> Boolean
; Has the worm hit the wall?
(define (hit-wall? ws)
  (cond
    ; position is outside the canvas in x-direction
    [(or (< (- (worm-x ws) RADIUS) 0)
         (> (+ (worm-x ws) RADIUS) (image-width BACKGROUND)))
     #true]
    ; position is outside the canvas is y-direction
    [(or (< (- (worm-y ws) RADIUS) 0)
         (> (+ (worm-y ws) RADIUS) (image-height BACKGROUND)))
     #true]
    [else #false]))

; outside the canvas in x-direction
(check-expect (hit-wall? (make-ws (make-posn (- RADIUS 1)
                                             (* 1/2 (image-height BACKGROUND)))
                                  (make-posn -1 0)))
              #true)
(check-expect (hit-wall? (make-ws (make-posn (- (image-width BACKGROUND)
                                                (- RADIUS 1))
                                             (* 1/2 (image-height BACKGROUND)))
                                  (make-posn 1 0)))
              #true)
; outside the canvas in y-direction
(check-expect (hit-wall? (make-ws (make-posn (* 1/2 (image-width BACKGROUND))
                                             (- RADIUS 1))
                                  (make-posn 0 -1)))
              #true)
(check-expect (hit-wall? (make-ws (make-posn (* 1/2 (image-width BACKGROUND))
                                             (- (image-height BACKGROUND)
                                                (- RADIUS 1)))
                                  (make-posn 0 1)))
              #true)
; middle of the canvas
(check-expect (hit-wall? (make-ws (make-posn (* 1/2 (image-width BACKGROUND))
                                             (* 1/2 (image-height BACKGROUND)))
                                  (make-posn 1 0)))
              #false)

; WorldState -> Image
; Render the final image of the state
; of the world after the program has stopped.
(define (render-final ws)
  (place-image GAME-OVER
               (+ (* 1/2 (image-width GAME-OVER))
                  (* 1 RADIUS))
               (- (image-height BACKGROUND)
                  (* 1/2 (image-height GAME-OVER))
                  (* 0 RADIUS))
               (render ws)))

(check-expect (render-final (make-ws (make-posn 50 75) (make-posn 1 0)))
              (place-image GAME-OVER
                           (+ (* 1/2 (image-width GAME-OVER))
                              (* 1 RADIUS))
                           (- (image-height BACKGROUND)
                              (* 1/2 (image-height GAME-OVER))
                              (* 0 RADIUS))
                           (render (make-ws (make-posn 50 75)
                                            (make-posn 1 0)))))

; WorldState -> Number
; Return the current x-position
; of the worm.
(define (worm-x ws)
  (posn-x (ws-pos ws)))

(check-expect (worm-x (make-ws (make-posn 75 100) (make-posn 1 0)))
              75)

; WorldState -> Number
; Return the current y-position
; of the worm.
(define (worm-y ws)
  (posn-y (ws-pos ws)))

(check-expect (worm-y (make-ws (make-posn 75 100) (make-posn 1 0)))
              100)

; WorldState -> Number
; Return the current x-direction
; of the worm.
(define (worm-x-dir ws)
  (posn-x (ws-dir ws)))

(check-expect (worm-x-dir (make-ws (make-posn 75 100) (make-posn 1 0)))
              1)

; WorldState -> Number
; Return the current y-direction
; of the worm.
(define (worm-y-dir ws)
  (posn-y (ws-dir ws)))

(check-expect (worm-y-dir (make-ws (make-posn 75 100) (make-posn 1 0)))
              0)

