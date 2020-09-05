;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex217) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

; A NE-WorldState (non-empty world state) is one of:
; – (cons WormSegment '())
; – (cons WormSegment NE-WorldState)
; Interpretation:
;     The segment(s) of the worm,
;     tail first, head last.

; CONSTANTS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define RADIUS 5)                          ; radius of one segment of the worm
(define WIDTH (min 1000 (* 40 RADIUS)))    ; width of the canvas
(define HEIGHT (min 1250 (* 52 RADIUS)))   ; height of the canvas
(define SPEED (* 2 RADIUS))                ; the speed of the worm

(define WORM (circle RADIUS "solid" "red"))
(define BACKGROUND (empty-scene WIDTH HEIGHT "Midnight Blue"))

(define NUMBER-ERROR (string-append "The number must be an integer between "
                                    "1 and 10 inclusive."))

; for testing
(define TEST-WORM
  (list (make-worm (make-posn (+ (* 1/2 WIDTH) RADIUS)
                              (- (* 1/2 HEIGHT) (* 9 RADIUS)))
                   (make-dir 0 0))
        (make-worm (make-posn (+ (* 1/2 WIDTH) RADIUS)
                              (- (* 1/2 HEIGHT) (* 7 RADIUS)))
                   (make-dir 0 0))
        (make-worm (make-posn (+ (* 1/2 WIDTH) RADIUS)
                              (- (* 1/2 HEIGHT) (* 5 RADIUS)))
                   (make-dir 0 0))
        (make-worm (make-posn (+ (* 1/2 WIDTH) RADIUS)
                              (- (* 1/2 HEIGHT) (* 3 RADIUS)))
                   (make-dir 0 0))
        (make-worm (make-posn (+ (* 1/2 WIDTH) RADIUS)
                              (- (* 1/2 HEIGHT) RADIUS))
                   (make-dir 0 0))))

(define TEST-WORM-2
  (list (make-worm (make-posn (+ (* 1/2 WIDTH) RADIUS)
                              (- (* 1/2 HEIGHT) (* 9 RADIUS)))
                   (make-dir 0 -1))
        (make-worm (make-posn (+ (* 1/2 WIDTH) RADIUS)
                              (- (* 1/2 HEIGHT) (* 7 RADIUS)))
                   (make-dir 0 -1))
        (make-worm (make-posn (+ (* 1/2 WIDTH) RADIUS)
                              (- (* 1/2 HEIGHT) (* 5 RADIUS)))
                   (make-dir 0 -1))
        (make-worm (make-posn (+ (* 1/2 WIDTH) RADIUS)
                              (- (* 1/2 HEIGHT) (* 3 RADIUS)))
                   (make-dir 0 -1))
        (make-worm (make-posn (+ (* 1/2 WIDTH) RADIUS)
                              (- (* 1/2 HEIGHT) RADIUS))
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
    [on-tick move-worm s]))

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
         (set-head-dir (set-dir news (make-dir 0 1))
                       (make-dir 1 0))
         (set-head-dir news (make-dir 1 0)))]
    [(key=? ke "left")
     (if (dir-not-set? news)
         (set-head-dir (set-dir news (make-dir 0 1))
                       (make-dir -1 0))
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
              (list (make-worm (worm-pos (first TEST-WORM)) (make-dir 0 1))
                    (make-worm (worm-pos (second TEST-WORM)) (make-dir 0 1))
                    (make-worm (worm-pos (third TEST-WORM)) (make-dir 0 1))
                    (make-worm (worm-pos (fourth TEST-WORM)) (make-dir 0 1))
                    (make-worm (worm-pos (fifth TEST-WORM)) (make-dir 1 0))))
(check-expect (ke-handler TEST-WORM "left")
              (list (make-worm (worm-pos (first TEST-WORM)) (make-dir 0 1))
                    (make-worm (worm-pos (second TEST-WORM)) (make-dir 0 1))
                    (make-worm (worm-pos (third TEST-WORM)) (make-dir 0 1))
                    (make-worm (worm-pos (fourth TEST-WORM)) (make-dir 0 1))
                    (make-worm (worm-pos (fifth TEST-WORM)) (make-dir -1 0))))

(check-expect (ke-handler TEST-WORM-2 "up")
              (list (first TEST-WORM-2)
                    (second TEST-WORM-2)
                    (third TEST-WORM-2)
                    (fourth TEST-WORM-2)
                    (make-worm (worm-pos (fifth TEST-WORM-2)) (make-dir 0 -1))))
(check-expect (ke-handler TEST-WORM-2 "down")
              (list (first TEST-WORM-2)
                    (second TEST-WORM-2)
                    (third TEST-WORM-2)
                    (fourth TEST-WORM-2)
                    (make-worm (worm-pos (fifth TEST-WORM-2)) (make-dir 0 1))))
(check-expect (ke-handler TEST-WORM-2 "right")
              (list (first TEST-WORM-2)
                    (second TEST-WORM-2)
                    (third TEST-WORM-2)
                    (fourth TEST-WORM-2)
                    (make-worm (worm-pos (fifth TEST-WORM-2)) (make-dir 1 0))))
(check-expect (ke-handler TEST-WORM-2 "left")
              (list (first TEST-WORM-2)
                    (second TEST-WORM-2)
                    (third TEST-WORM-2)
                    (fourth TEST-WORM-2)
                    (make-worm (worm-pos (fifth TEST-WORM-2)) (make-dir -1 0))))

(check-expect (ke-handler TEST-WORM " ")
              TEST-WORM)

; NE-WorldState -> NE-WorldState
; Change the position of the worm
; every clock tick according to its
; directional vector.
(define (move-worm news)
  (cond
    [(empty? (rest news))
     (list (make-worm
            (make-posn (+ (worm-x (first news))
                          (* SPEED (worm-x-dir (first news))))
                       (+ (worm-y (first news))
                          (* SPEED (worm-y-dir (first news)))))
            (worm-dir (first news))))]
    [else
     (append
      (list
       (make-worm
        (make-posn (+ (worm-x (first news))
                      (* SPEED (worm-x-dir (first news))))
                   (+ (worm-y (first news))
                      (* SPEED (worm-y-dir (first news)))))
        (worm-dir (second news))))
      (move-worm (rest news)))]))

(check-expect (move-worm (list (make-worm (make-posn 10 10) (make-dir 1 0))
                               (make-worm (make-posn 20 10) (make-dir 0 1))
                               (make-worm (make-posn 20 20) (make-dir -1 0))))
              (list (make-worm (make-posn (+ 10 SPEED) 10) (make-dir 0 1))
                    (make-worm (make-posn 20 (+ 10 SPEED)) (make-dir -1 0))
                    (make-worm (make-posn (- 20 SPEED) 20) (make-dir -1 0))))

; Number -> NE-WorldState
; Create a worm with n segments.
(define (make-worm-segments n)
  (cond
    [(<= n 0) '()]
    [(> n 0)
     (cons
      (make-worm (make-posn (+ (* 1/2 WIDTH) RADIUS)
                            (- (* 1/2 HEIGHT) (* RADIUS (- (* 2 n) 1))))
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
  (cond
    [(empty? (rest news))
     (list (make-worm (worm-pos (first news)) dirvec))]
    [else
     (append (list (first news))
             (set-head-dir (rest news) dirvec))]))

(check-expect (set-head-dir TEST-WORM (make-dir 0 -1))
              (list (first TEST-WORM)
                    (second TEST-WORM)
                    (third TEST-WORM)
                    (fourth TEST-WORM)
                    (make-worm (worm-pos (fifth TEST-WORM)) (make-dir 0 -1))))

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

