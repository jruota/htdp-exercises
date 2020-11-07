;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex224) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; CONSTANTS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; graphical

(define WIDTH 700)
(define HEIGHT (* 9/7 WIDTH))

(define TANK-BAR (rectangle (* 1/14 WIDTH) (* 1/90 HEIGHT) "solid" "palegreen"))
(define TANK-TIP (isosceles-triangle (* 1/45 WIDTH) 110 "solid" "palegreen"))
; (define TANK (above TANK-TIP TANK-BAR))
(define TANK (overlay/offset TANK-TIP
                             0
                             (- (image-height TANK-TIP) 1)
                             TANK-BAR))

(define SPACE-SHIP-BAR (rectangle (* 1/14 WIDTH)
                                  (* 1/90 HEIGHT)
                                  "solid"
                                  "pink"))
(define SPACE-SHIP-COL (rectangle (* 1/70 WIDTH)
                                  (* 1/90 HEIGHT)
                                  "solid"
                                  "pink"))
(define SPACESHIP
  (overlay/align/offset
   "middle"
   "top"
   (overlay/align/offset
    "left"
    "top"
    (overlay/align/offset
     "right"
     "top"
     SPACE-SHIP-BAR
     0
     (* 1/90 HEIGHT)
     SPACE-SHIP-COL)
    0
    (* 1/90 HEIGHT)
    SPACE-SHIP-COL)
   0
   (* 1/90 HEIGHT)
   SPACE-SHIP-COL))

(define ROCKET (triangle (* 1/70 WIDTH) "solid" "red"))
(define LASER-BEAM (rectangle 4 (* 2/90 HEIGHT) "solid" "fuchsia"))

(define OBSTACLE-WIDTH (* 1/14 WIDTH))
(define OBSTACLE-HEIGHT (* 5/90 HEIGHT))
(define OBSTACLE (rectangle OBSTACLE-WIDTH OBSTACLE-HEIGHT
                            "solid" "transparent"))
(define OBSTACLE-COLOR "lightsteelblue")

(define LIFE (overlay (circle (- (* 2/140 WIDTH) 2) "solid" "red")
                      (square (* 2/70 WIDTH) "solid" "transparent")))

(define BACKGROUND (empty-scene WIDTH HEIGHT "midnight blue"))

; "physical"

(define TANK-LIVES 3)
(define TANK-Y (- HEIGHT (image-height TANK)))

(define SPACE-SHIP-LIVES 1)
(define HSSD (* 7/5 (image-width SPACESHIP)))  ; horizontal space-ship distance
(define VSSD (* 2 (image-height SPACESHIP)))   ; vertical space-ship distance

(define OBSTACLE-X-FIRST (+ (* 1/6 (- WIDTH (* 5 (image-width OBSTACLE))))
                            (* 1/2 (image-width OBSTACLE))))
(define OBSTACLE-X-NEXT (+ (* 1/6 (- WIDTH (* 5 (image-width OBSTACLE))))
                           (* 1 (image-width OBSTACLE))))
(define OBSTACLE-DAMAGE (* 1/90 HEIGHT))
(define OBSTACLE-Y (* 7/8 HEIGHT))

(define STRING-SIZE (* 2/70 WIDTH))
(define STRING-COLOR "red")
(define STRING-FACE "Ubuntu Mono")
(define STRING-FAMILY "modern")
(define STRING-STYLE "normal")
(define STRING-WEIGHT "bold")
(define STRING-UNDERLINE #f)
(define TEXT-Y 24)

(define UFO-SPEED 3)
; (define UFO-JUMP 13)    ; make this a positive, uneven number
(define ROCKET-SPEED (* 2 UFO-SPEED))
(define TANK-SPEED UFO-SPEED)

; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; the data definition needs to keep track of
;     – the tank
;     – any rockets fired by the tank
;     – the amount of lives the tank has
;     – the spce ships
;     – any lasers fired by the space ships
;     – the points
;     – any obstacles

(define-struct tank [xpos vel lives])
; A Tank is a structure:
;     (make-tank Number Number Number)
; Interpretation:
;     The current x-position of the tank,
;     it velocity and the amount of lives left.

; A Rocket is a Posn:
;     (make-posn Number Number)
; Interpretation:
;     The current position of a fired rocket.

; A LoR (list of rockets) is one of:
;     – '()
;     – (cons Rocket LoR)
; Interpretation:
;     A list of all active, fired rockets.

(define-struct spaceship [xpos ypos lives])
; A SpaceShip is a structure:
;     (make-spaceship Number Number Number)
; Interpretation:
;     The current x- and y-position of a space-ship
;     as well as its lives left.

; A SSC (space-ship column) is one of:
;     – '()
;     – (cons SpaceShip SSC)
; Interpretation:
;     A list of vertically aligned space-ships,
;     lowest to highest.

; A SSM (space-ship matrix) is one of:
;     – '()
;     – (cons SSC SSR)
; Interpretation:
;     A matrix of vertically and horizontally aligned space-ships.
;     The elements of the matrix are its columns

; A Laser is a Posn:
;     (make-posn Number Number)
; Interpretation:
;     The current position of a fired
;     laser beam.

; A LoL (list of lasers) is one of:
;     – '()
;     – (cons Laser LoL)
; Interpretation:
;     A list of all fired laser beams.

(define-struct obstacle [xpos height])
; An Obstacle is a structure:
;     (make-obstacle Number Number)
; Interpretation:
;     The x-position and the height of an
;     obstacle.

; An LoO (list of obstacles) is one of:
;     – '()
;     – (cons Obstacle LoO)
; Interpretation:
;     A list of obstacles.

(define-struct ws [tank lor ssm lol loo points])
; A WorldState is a structure:
;     (make-ws Tank LoR SSM LoL LoO Number)
; Interpretation:
;     Combines all elements of the game
;     as well as the current points.

; DATA EXAMPLES ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define POINTS-TEXT (text "Points: 250" 24 "red"))
(define LIVES-TEXT (text "Lives: 3" 24 "red"))

(define SSC-1 (list
               (make-spaceship (- (* 1/2 WIDTH) (* 2 HSSD))
                               (* 2 VSSD)
                               SPACE-SHIP-LIVES)
               (make-spaceship (- (* 1/2 WIDTH) (* 2 HSSD))
                               (* 3 VSSD)
                               SPACE-SHIP-LIVES)
               (make-spaceship (- (* 1/2 WIDTH) (* 2 HSSD))
                               (* 4 VSSD)
                               SPACE-SHIP-LIVES)
               (make-spaceship (- (* 1/2 WIDTH) (* 2 HSSD))
                               (* 5 VSSD)
                               SPACE-SHIP-LIVES)
               (make-spaceship (- (* 1/2 WIDTH) (* 2 HSSD))
                               (* 6 VSSD)
                               SPACE-SHIP-LIVES)))
(define SSC-2 (list
               (make-spaceship (- (* 1/2 WIDTH) (* 1 HSSD))
                               (* 2 VSSD)
                               SPACE-SHIP-LIVES)
               (make-spaceship (- (* 1/2 WIDTH) (* 1 HSSD))
                               (* 3 VSSD)
                               SPACE-SHIP-LIVES)
               (make-spaceship (- (* 1/2 WIDTH) (* 1 HSSD))
                               (* 4 VSSD)
                               SPACE-SHIP-LIVES)
               (make-spaceship (- (* 1/2 WIDTH) (* 1 HSSD))
                               (* 5 VSSD)
                               SPACE-SHIP-LIVES)
               (make-spaceship (- (* 1/2 WIDTH) (* 1 HSSD))
                               (* 6 VSSD)
                               SPACE-SHIP-LIVES)))
(define SSC-3 (list
               (make-spaceship (- (* 1/2 WIDTH) (* 0 HSSD))
                               (* 2 VSSD)
                               SPACE-SHIP-LIVES)
               (make-spaceship (- (* 1/2 WIDTH) (* 0 HSSD))
                               (* 3 VSSD)
                               SPACE-SHIP-LIVES)
               (make-spaceship (- (* 1/2 WIDTH) (* 0 HSSD))
                               (* 4 VSSD)
                               SPACE-SHIP-LIVES)
               (make-spaceship (- (* 1/2 WIDTH) (* 0 HSSD))
                               (* 5 VSSD)
                               SPACE-SHIP-LIVES)
               (make-spaceship (- (* 1/2 WIDTH) (* 0 HSSD))
                               (* 6 VSSD)
                               SPACE-SHIP-LIVES)))
(define SSC-4 (list
               (make-spaceship (+ (* 1/2 WIDTH) (* 1 HSSD))
                               (* 2 VSSD)
                               SPACE-SHIP-LIVES)
               (make-spaceship (+ (* 1/2 WIDTH) (* 1 HSSD))
                               (* 3 VSSD)
                               SPACE-SHIP-LIVES)
               (make-spaceship (+ (* 1/2 WIDTH) (* 1 HSSD))
                               (* 4 VSSD)
                               SPACE-SHIP-LIVES)
               (make-spaceship (+ (* 1/2 WIDTH) (* 1 HSSD))
                               (* 5 VSSD)
                               SPACE-SHIP-LIVES)
               (make-spaceship (+ (* 1/2 WIDTH) (* 1 HSSD))
                               (* 6 VSSD)
                               SPACE-SHIP-LIVES)))
(define SSC-5 (list
               (make-spaceship (+ (* 1/2 WIDTH) (* 2 HSSD))
                               (* 2 VSSD)
                               SPACE-SHIP-LIVES)
               (make-spaceship (+ (* 1/2 WIDTH) (* 2 HSSD))
                               (* 3 VSSD)
                               SPACE-SHIP-LIVES)
               (make-spaceship (+ (* 1/2 WIDTH) (* 2 HSSD))
                               (* 4 VSSD)
                               SPACE-SHIP-LIVES)
               (make-spaceship (+ (* 1/2 WIDTH) (* 2 HSSD))
                               (* 5 VSSD)
                               SPACE-SHIP-LIVES)
               (make-spaceship (+ (* 1/2 WIDTH) (* 2 HSSD))
                               (* 6 VSSD)
                               SPACE-SHIP-LIVES)))
(define SSM-EXAMPLE
  (list SSC-1 SSC-2 SSC-3 SSC-4 SSC-5))

(define WS1-EXAMPLE
  (make-ws (make-tank (* 1/2 WIDTH) 0 3)                    ; tank
           (list (make-posn (* 1/2 WIDTH) (* 3/4 HEIGHT))   ; rocket
                 (make-posn (* 1/2 WIDTH) (* 2/4 HEIGHT)))  ; rocket
           SSM-EXAMPLE                                      ; space-ships
           (list (make-posn (- (* 1/2 WIDTH) (* 2 HSSD)) (* 7 VSSD))  ; laser
                 (make-posn (+ (* 1/2 WIDTH) (* 2 HSSD)) (* 9 VSSD))) ; laser
           (list
            (make-obstacle OBSTACLE-X-FIRST
                           OBSTACLE-HEIGHT)
            (make-obstacle (+ OBSTACLE-X-FIRST OBSTACLE-X-NEXT)
                           OBSTACLE-HEIGHT)
            (make-obstacle (+ OBSTACLE-X-FIRST (* 2 OBSTACLE-X-NEXT))
                           OBSTACLE-HEIGHT)
            (make-obstacle (+ OBSTACLE-X-FIRST (* 3 OBSTACLE-X-NEXT))
                           OBSTACLE-HEIGHT)
            (make-obstacle (+ OBSTACLE-X-FIRST (* 4 OBSTACLE-X-NEXT))
                           OBSTACLE-HEIGHT))
           250))                                            ; points

(define WS2-EXAMPLE
  (make-ws (make-tank (* 1/2 WIDTH) (* -1 TANK-SPEED) 3)    ; tank
           (list (make-posn (* 1/2 WIDTH) (* 3/4 HEIGHT))   ; rocket
                 (make-posn (* 1/2 WIDTH) (* 2/4 HEIGHT)))  ; rocket
           SSM-EXAMPLE                                      ; space-ships
           (list (make-posn (- (* 1/2 WIDTH) (* 2 HSSD)) (* 7 VSSD))  ; laser
                 (make-posn (+ (* 1/2 WIDTH) (* 2 HSSD)) (* 9 VSSD))) ; laser
           (list
            (make-obstacle OBSTACLE-X-FIRST
                           OBSTACLE-HEIGHT)
            (make-obstacle (+ OBSTACLE-X-FIRST OBSTACLE-X-NEXT)
                           OBSTACLE-HEIGHT)
            (make-obstacle (+ OBSTACLE-X-FIRST (* 2 OBSTACLE-X-NEXT))
                           OBSTACLE-HEIGHT)
            (make-obstacle (+ OBSTACLE-X-FIRST (* 3 OBSTACLE-X-NEXT))
                           OBSTACLE-HEIGHT)
            (make-obstacle (+ OBSTACLE-X-FIRST (* 4 OBSTACLE-X-NEXT))
                           OBSTACLE-HEIGHT))
           250))                                            ; points

(define WS3-EXAMPLE
  (make-ws (make-tank (* 1/2 WIDTH) (abs TANK-SPEED) 3)     ; tank
           (list (make-posn (* 1/2 WIDTH) (* 3/4 HEIGHT))   ; rocket
                 (make-posn (* 1/2 WIDTH) (* 2/4 HEIGHT)))  ; rocket
           SSM-EXAMPLE                                      ; space-ships
           (list (make-posn (- (* 1/2 WIDTH) (* 2 HSSD)) (* 7 VSSD))  ; laser
                 (make-posn (+ (* 1/2 WIDTH) (* 2 HSSD)) (* 9 VSSD))) ; laser
           (list
            (make-obstacle OBSTACLE-X-FIRST
                           OBSTACLE-HEIGHT)
            (make-obstacle (+ OBSTACLE-X-FIRST OBSTACLE-X-NEXT)
                           OBSTACLE-HEIGHT)
            (make-obstacle (+ OBSTACLE-X-FIRST (* 2 OBSTACLE-X-NEXT))
                           OBSTACLE-HEIGHT)
            (make-obstacle (+ OBSTACLE-X-FIRST (* 3 OBSTACLE-X-NEXT))
                           OBSTACLE-HEIGHT)
            (make-obstacle (+ OBSTACLE-X-FIRST (* 4 OBSTACLE-X-NEXT))
                           OBSTACLE-HEIGHT))
           250))                                            ; points

(define WS4-EXAMPLE
  (make-ws (make-tank (* 1/2 WIDTH) 0 3)                    ; tank
           (list (make-posn (* 1/2 WIDTH) (* 3/4 HEIGHT))   ; rockets
                 (make-posn (* 1/2 WIDTH) (* 2/4 HEIGHT))
                 (make-posn (- (* 1/2 WIDTH)
                               (* 2 OBSTACLE-X-NEXT)
                               (* 1/2 (image-width OBSTACLE)))
                            (+ OBSTACLE-Y (* 2 OBSTACLE-DAMAGE)))
                 (make-posn (- (* 1/2 WIDTH)
                               (* 2 OBSTACLE-X-NEXT)
                               (* 0 (image-width OBSTACLE)))
                            (+ OBSTACLE-Y (* 2 OBSTACLE-DAMAGE)))
                 (make-posn (- (* 1/2 WIDTH)
                               (* 2 OBSTACLE-X-NEXT)
                               (* -1/2 (image-width OBSTACLE)))
                            (+ OBSTACLE-Y (* 2 OBSTACLE-DAMAGE)))
                 (make-posn (- (* 1/2 WIDTH)
                               OBSTACLE-X-NEXT
                               (* 1/2 (image-width OBSTACLE)))
                            OBSTACLE-Y)
                 (make-posn (- (* 1/2 WIDTH)
                               OBSTACLE-X-NEXT
                               (* 0 (image-width OBSTACLE)))
                            OBSTACLE-Y)
                 (make-posn (- (* 1/2 WIDTH)
                               OBSTACLE-X-NEXT
                               (* -1/2 (image-width OBSTACLE)))
                            OBSTACLE-Y)
                 (make-posn (- (* 1/2 WIDTH) (* 1/2 (image-width OBSTACLE)))
                            (+ OBSTACLE-Y (* 5/2 OBSTACLE-DAMAGE)))
                 (make-posn (* 1/2 WIDTH)
                            (+ OBSTACLE-Y (* 5/2 OBSTACLE-DAMAGE)))
                 (make-posn (+ (* 1/2 WIDTH) (* 1/2 (image-width OBSTACLE)))
                            (+ OBSTACLE-Y (* 5/2 OBSTACLE-DAMAGE)))
                 (make-posn (- (* 1/2 WIDTH) (* 1/2 (image-width OBSTACLE)))
                            (+ OBSTACLE-Y 0))
                 (make-posn (* 1/2 WIDTH)
                            (+ OBSTACLE-Y 0))
                 (make-posn (+ (* 1/2 WIDTH) (* 1/2 (image-width OBSTACLE)))
                            (+ OBSTACLE-Y 0))
                 (make-posn (- (* 1/2 WIDTH) (* 1/2 (image-width OBSTACLE)))
                            (- OBSTACLE-Y (* 5/2 OBSTACLE-DAMAGE)))
                 (make-posn (* 1/2 WIDTH)
                            (- OBSTACLE-Y (* 5/2 OBSTACLE-DAMAGE)))
                 (make-posn (+ (* 1/2 WIDTH) (* 1/2 (image-width OBSTACLE)))
                            (- OBSTACLE-Y (* 5/2 OBSTACLE-DAMAGE)))
                 (make-posn (- (* 1/2 WIDTH)
                               (* 2 HSSD)
                               (* 1/2 (image-width SPACESHIP)))
                            (+ (* 6 VSSD)
                               (* 1/2 (image-height SPACESHIP))))
                 (make-posn (- (* 1/2 WIDTH)
                               (* 2 HSSD)
                               (* 0 (image-width SPACESHIP)))
                            (+ (* 6 VSSD)
                               (* 1/2 (image-height SPACESHIP))))
                 (make-posn (- (* 1/2 WIDTH)
                               (* 2 HSSD)
                               (* -1/2 (image-width SPACESHIP)))
                            (+ (* 6 VSSD)
                               (* 1/2 (image-height SPACESHIP))))
                 (make-posn (- (* 1/2 WIDTH)
                               (* 1 HSSD)
                               (* 1/2 (image-width SPACESHIP)))
                            (+ (* 6 VSSD)
                               (* 0 (image-height SPACESHIP))))
                 (make-posn (- (* 1/2 WIDTH)
                               (* 1 HSSD)
                               (* 0 (image-width SPACESHIP)))
                            (+ (* 6 VSSD)
                               (* 0 (image-height SPACESHIP))))
                 (make-posn (- (* 1/2 WIDTH)
                               (* 1 HSSD)
                               (* -1/2 (image-width SPACESHIP)))
                            (+ (* 6 VSSD)
                               (* 0 (image-height SPACESHIP))))
                 (make-posn (- (* 1/2 WIDTH)
                               (* 0 HSSD)
                               (* 1/2 (image-width SPACESHIP)))
                            (- (* 6 VSSD)
                               (* 1/2 (image-height SPACESHIP))))
                 (make-posn (- (* 1/2 WIDTH)
                               (* 0 HSSD)
                               (* 0 (image-width SPACESHIP)))
                            (- (* 6 VSSD)
                               (* 1/2 (image-height SPACESHIP))))
                 (make-posn (- (* 1/2 WIDTH)
                               (* 0 HSSD)
                               (* -1/2 (image-width SPACESHIP)))
                            (- (* 6 VSSD)
                               (* 1/2 (image-height SPACESHIP)))))
           SSM-EXAMPLE                                      ; space-ships
           (list (make-posn (- (* 1/2 WIDTH) (* 2 HSSD)) (* 7 VSSD))  ; laser
                 (make-posn (+ (* 1/2 WIDTH) (* 2 HSSD)) (* 9 VSSD))) ; laser
           (list (make-obstacle (- (* 1/2 WIDTH) (* 2 OBSTACLE-X-NEXT))
                                (* 1/90 HEIGHT))
                 (make-obstacle (- (* 1/2 WIDTH) OBSTACLE-X-NEXT)
                                (* 2/90 HEIGHT))
                 (make-obstacle (* 1/2 WIDTH)
                                (* 5/90 HEIGHT))
                 (make-obstacle (+ (* 1/2 WIDTH) OBSTACLE-X-NEXT)
                                (* 4/90 HEIGHT))
                 (make-obstacle (+ (* 1/2 WIDTH) (* 2 OBSTACLE-X-NEXT))
                                (* 3/90 HEIGHT)))
           250))                                            ; points

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; ??? -> WorldState
; Start the main program.
(define (full-space-war a)
  (big-bang (make-ws
             (make-tank (* 1/2 (image-width BACKGROUND)) 0 TANK-LIVES)
             '()
             '()
             '()
             (list
              (make-obstacle OBSTACLE-X-FIRST
                             OBSTACLE-HEIGHT)
              (make-obstacle (+ OBSTACLE-X-FIRST OBSTACLE-X-NEXT)
                             OBSTACLE-HEIGHT)
              (make-obstacle (+ OBSTACLE-X-FIRST (* 2 OBSTACLE-X-NEXT))
                             OBSTACLE-HEIGHT)
              (make-obstacle (+ OBSTACLE-X-FIRST (* 3 OBSTACLE-X-NEXT))
                             OBSTACLE-HEIGHT)
              (make-obstacle (+ OBSTACLE-X-FIRST (* 4 OBSTACLE-X-NEXT))
                             OBSTACLE-HEIGHT))
             0)
    [on-draw render]
    [on-key ke-handler]
    [on-release release-handler]
    [on-tick tock]))

; RENDERING FUNCTIONS ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

; WorldState -> Image
; Render the current world state as an image.
(define (render ws)
  (render-lives
   (ws-tank ws)
   (render-points
    (ws-points ws)
    (render-rockets
     (ws-lor ws)
     (render-lasers
      (ws-lol ws)
      (render-spaceships
       (ws-ssm ws)
       (render-tank
        (ws-tank ws)
        (render-obstacles
         (ws-loo ws)
         BACKGROUND))))))))

; TANK Image -> Image
; Render the tank t into image img.
(define (render-tank t img)
  (place-image TANK
               (tank-xpos t)
               TANK-Y
               img))

(check-expect (render-tank (make-tank (* 1/2 WIDTH) 0 3) BACKGROUND)
              (place-image TANK
                           (* 1/2 WIDTH)
                           TANK-Y
                           BACKGROUND))

; LoR Image -> Image
; Render all rockets in lor into image img.
(define (render-rockets lor img)
  (cond
    [(empty? lor) img]
    [(cons? lor)
     (render-rocket (first lor)
                    (render-rockets (rest lor)
                                    img))]))

(check-expect (render-rockets '() BACKGROUND)
              BACKGROUND)
(check-expect (render-rockets (list
                               (make-posn 100 100)
                               (make-posn 200 200)
                               (make-posn 300 300))
                              BACKGROUND)
              (place-image
               ROCKET
               100 100
               (place-image
                ROCKET
                200 200
                (place-image
                 ROCKET
                 300 300
                 BACKGROUND))))

; Rocket Image -> Image
; Render the rocket r into image img.
(define (render-rocket r img)
  (place-image ROCKET
               (posn-x r)
               (posn-y r)
               img))

(check-expect (render-rocket (make-posn (* 1/2 WIDTH) (* 1/2 HEIGHT))
                             BACKGROUND)
              (place-image ROCKET
                           (* 1/2 WIDTH)
                           (* 1/2 HEIGHT)
                           BACKGROUND))

; SSM Image -> Image
; Render all space-ships in ssm
; into the image img.
(define (render-spaceships ssm img)
  (cond
    [(empty? ssm) img]
    [(cons? ssm)
     (render-spaceship-column (first ssm)
                               (render-spaceships (rest ssm) img))]))

(check-expect (render-spaceships '() BACKGROUND)
              BACKGROUND)
(check-expect (render-spaceships SSM-EXAMPLE BACKGROUND)
              (render-spaceship-column
               SSC-1
               (render-spaceship-column
                SSC-2
                (render-spaceship-column
                 SSC-3
                 (render-spaceship-column
                  SSC-4
                  (render-spaceship-column
                   SSC-5
                   BACKGROUND))))))

; SSC Image -> Image
; Render all space-ships in ssc
; into the image img.
(define (render-spaceship-column ssc img)
  (cond
    [(empty? ssc) img]
    [(cons? ssc)
     (render-spaceship (first ssc)
                        (render-spaceship-column (rest ssc) img))]))

(check-expect (render-spaceship-column '() BACKGROUND)
              BACKGROUND)
(check-expect (render-spaceship-column SSC-1 BACKGROUND)
              (render-spaceship
               (first SSC-1)
               (render-spaceship
                (second SSC-1)
                (render-spaceship
                 (third SSC-1)
                 (render-spaceship
                  (fourth SSC-1)
                  (render-spaceship
                   (fifth SSC-1)
                   BACKGROUND))))))

; SpaceShip Image -> Image
; Render the space-ship ss into
; the image img.
(define (render-spaceship ss img)
  (place-image SPACESHIP
               (spaceship-xpos ss)
               (spaceship-ypos ss)
               img))

(check-expect (render-spaceship
               (make-spaceship (* 1/2 WIDTH)
                               (* 1/2 HEIGHT)
                               1)
               BACKGROUND)
              (place-image SPACESHIP
                           (* 1/2 WIDTH)
                           (* 1/2 HEIGHT)
                           BACKGROUND))

; LoL Image -> Image
; Render all lasers in lol into
; the image img.
(define (render-lasers lol img)
  (cond
    [(empty? lol) img]
    [(cons? lol)
     (render-laser (first lol)
                   (render-lasers (rest lol) img))]))

(check-expect (render-lasers '() BACKGROUND)
              BACKGROUND)
(check-expect (render-lasers (list
                              (make-posn 100 100)
                              (make-posn 150 150)
                              (make-posn 200 200))
                             BACKGROUND)
              (place-image
               LASER-BEAM
               100
               100
               (place-image
                LASER-BEAM
                150
                150
                (place-image
                 LASER-BEAM
                 200
                 200
                 BACKGROUND))))

; Laser Image -> Image
; Render the laser l into the image img.
(define (render-laser l img)
  (place-image LASER-BEAM
               (posn-x l)
               (posn-y l)
               img))

(check-expect (render-laser (make-posn 250 250) BACKGROUND)
              (place-image
               LASER-BEAM
               250
               250
               BACKGROUND))

; LoO Image -> Image
; Render all obstacles in loo into image img.
(define (render-obstacles loo img)
  (cond
    [(empty? loo) img]
    [(cons? loo)
     (place-image (render-obstacle (obstacle-height (first loo)))
                  (obstacle-xpos (first loo))
                  OBSTACLE-Y
                  (render-obstacles (rest loo) img))]))

(check-expect (render-obstacles
               (list
                (make-obstacle (- (* 1/2 WIDTH) (* 2 OBSTACLE-X-NEXT))
                               (* 1/90 HEIGHT))
                (make-obstacle (- (* 1/2 WIDTH) OBSTACLE-X-NEXT)
                               (* 2/90 HEIGHT))
                (make-obstacle (* 1/2 WIDTH)
                               (* 5/90 HEIGHT))
                (make-obstacle (+ (* 1/2 WIDTH) OBSTACLE-X-NEXT)
                               (* 4/90 HEIGHT))
                (make-obstacle (+ (* 1/2 WIDTH) (* 2 OBSTACLE-X-NEXT))
                               (* 3/90 HEIGHT)))
               BACKGROUND)
              (place-image
               (render-obstacle (* 1/90 HEIGHT))
               (- (* 1/2 WIDTH) (* 2 OBSTACLE-X-NEXT))
               OBSTACLE-Y
               (place-image
                (render-obstacle (* 2/90 HEIGHT))
                (- (* 1/2 WIDTH) OBSTACLE-X-NEXT)
                OBSTACLE-Y
                (place-image
                 (render-obstacle (* 5/90 HEIGHT))
                 (* 1/2 WIDTH)
                 OBSTACLE-Y
                 (place-image
                  (render-obstacle (* 4/90 HEIGHT))
                  (+ (* 1/2 WIDTH) OBSTACLE-X-NEXT)
                  OBSTACLE-Y
                  (place-image
                   (render-obstacle (* 3/90 HEIGHT))
                   (+ (* 1/2 WIDTH) (* 2 OBSTACLE-X-NEXT))
                   OBSTACLE-Y
                   BACKGROUND))))))


; The actual obstacle is overlayed onto a possible larger but transparent one,
; so that damaged obstacles do not shift upwards. This is because the obstacles
; are placed according to their midpoint and that point would change when the
; obstacles are damaged and become smaller.

; Number -> Image
; Render an image representation of an obstacle with height h.
(define (render-obstacle h)
  (overlay/align
   "middle"
   "bottom"
   (rectangle OBSTACLE-WIDTH h "solid" OBSTACLE-COLOR)
   OBSTACLE))

(check-expect (render-obstacle (* 5/90 HEIGHT))
              (overlay/align
               "middle"
               "bottom"
               (rectangle OBSTACLE-WIDTH (* 5/90 HEIGHT) "solid" OBSTACLE-COLOR)
               OBSTACLE))

; Number Image -> Image
; Render the player's current points p into
; the image img.
(define (render-points p img)
  (place-image (text/font
                (string-append "POINTS: "
                               (number->string p))
                STRING-SIZE
                STRING-COLOR
                STRING-FACE
                STRING-FAMILY
                STRING-STYLE
                STRING-WEIGHT
                STRING-UNDERLINE)
               (* 8/10 WIDTH)
               TEXT-Y
               img))

(check-expect (render-points 250 BACKGROUND)
              (place-image (text/font
                            "POINTS: 250"
                            STRING-SIZE
                            STRING-COLOR
                            STRING-FACE
                            STRING-FAMILY
                            STRING-STYLE
                            STRING-WEIGHT
                            STRING-UNDERLINE)
                           (* 8/10 WIDTH)
                           TEXT-Y
                           BACKGROUND))

; Tank Image -> Image
; Render the remaining lives of the tank
; into image img.
(define (render-lives t img)
  (place-image
   (beside (text/font
            "LIVES: "
            STRING-SIZE
            STRING-COLOR
            STRING-FACE
            STRING-FAMILY
            STRING-STYLE
            STRING-WEIGHT
            STRING-UNDERLINE)
           (life-dots (tank-lives t)))
           (* 2/10 WIDTH)
           TEXT-Y
           img))

(check-expect (render-lives (make-tank 250 0 3) BACKGROUND)
              (place-image
               (beside
                (text/font
                 "LIVES: "
                 STRING-SIZE
                 STRING-COLOR
                 STRING-FACE
                 STRING-FAMILY
                 STRING-STYLE
                 STRING-WEIGHT
                 STRING-UNDERLINE)
                LIFE
                LIFE
                LIFE)
                (* 2/10 WIDTH)
                TEXT-Y
                BACKGROUND))

(check-expect (render-lives (make-tank 250 0 1) BACKGROUND)
              (place-image
               (beside
                (text/font
                 "LIVES: "
                 STRING-SIZE
                 STRING-COLOR
                 STRING-FACE
                 STRING-FAMILY
                 STRING-STYLE
                 STRING-WEIGHT
                 STRING-UNDERLINE)
                LIFE)
                (* 2/10 WIDTH)
                TEXT-Y
                BACKGROUND))

; Number -> Image
; Render n dots representing the remaining lives
; of the tank.
(define (life-dots n)
  (cond
    [(zero? n) empty-image]
    [(> n 0)
     (beside LIFE (life-dots (sub1 n)))]))

(check-expect (life-dots 0)
              empty-image)
(check-expect (life-dots 3)
              (beside LIFE LIFE LIFE))

; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

; KEY EVENT HANDLERs +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

; WorldState KeyEvent -> WorldState
; Deal with moving the tank and
; shooting rockets.
(define (ke-handler ws ke)
  (cond
    [(key=? ke " ")
     (make-ws
      (ws-tank ws)
      (cons
       (make-posn (tank-xpos (ws-tank ws)) TANK-Y)
       (ws-lor ws))
      (ws-ssm ws)
      (ws-lol ws)
      (ws-loo ws)
      (ws-points ws))]
    [(key=? ke "left")
     (make-ws
      (make-tank (tank-xpos (ws-tank ws))
                 (* -1 (abs TANK-SPEED))
                 (tank-lives (ws-tank ws)))
      (ws-lor ws)
      (ws-ssm ws)
      (ws-lol ws)
      (ws-loo ws)
      (ws-points ws))]
    [(key=? ke "right")
     (make-ws
      (make-tank (tank-xpos (ws-tank ws))
                 (abs TANK-SPEED)
                 (tank-lives (ws-tank ws)))
      (ws-lor ws)
      (ws-ssm ws)
      (ws-lol ws)
      (ws-loo ws)
      (ws-points ws))]
    [else ws]))

(check-expect (ke-handler WS1-EXAMPLE "a")
              WS1-EXAMPLE)

(check-expect (ke-handler WS1-EXAMPLE " ")
              (make-ws
               (ws-tank WS1-EXAMPLE)
               (cons
                (make-posn (tank-xpos (ws-tank WS1-EXAMPLE)) TANK-Y)
                (ws-lor WS1-EXAMPLE))
               (ws-ssm WS1-EXAMPLE)
               (ws-lol WS1-EXAMPLE)
               (ws-loo WS1-EXAMPLE)
               (ws-points WS1-EXAMPLE)))

(check-expect (ke-handler WS1-EXAMPLE "left")
              (make-ws
               (make-tank
                (tank-xpos (ws-tank WS1-EXAMPLE))
                (* -1 TANK-SPEED)
                (tank-lives (ws-tank WS1-EXAMPLE)))
               (ws-lor WS1-EXAMPLE)
               (ws-ssm WS1-EXAMPLE)
               (ws-lol WS1-EXAMPLE)
               (ws-loo WS1-EXAMPLE)
               (ws-points WS1-EXAMPLE)))

(check-expect (ke-handler WS1-EXAMPLE "right")
              (make-ws
               (make-tank
                (tank-xpos (ws-tank WS1-EXAMPLE))
                (abs TANK-SPEED)
                (tank-lives (ws-tank WS1-EXAMPLE)))
               (ws-lor WS1-EXAMPLE)
               (ws-ssm WS1-EXAMPLE)
               (ws-lol WS1-EXAMPLE)
               (ws-loo WS1-EXAMPLE)
               (ws-points WS1-EXAMPLE)))

; WorldState KeyEvent -> WorldState
; Stop the tank when the "left" or "right"
; key have been released.
(define (release-handler ws ke)
  (cond
    [(or (key=? ke "left")
         (key=? ke "right"))
     (make-ws
      (make-tank
       (tank-xpos (ws-tank ws))
       0
       (tank-lives (ws-tank ws)))
      (ws-lor ws)
      (ws-ssm ws)
      (ws-lol ws)
      (ws-loo ws)
      (ws-points ws))]
    [else ws]))

(check-expect (release-handler WS2-EXAMPLE "left")
              WS1-EXAMPLE)
(check-expect (release-handler WS3-EXAMPLE "right")
              WS1-EXAMPLE)
(check-expect (release-handler WS1-EXAMPLE " ")
              WS1-EXAMPLE)
              
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

; CLOCK TICK HANDLER +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

; Tasks for the clock tick handler:
; – move the space ships
; – move the tank
; – shoot and move the laser beams, remove laser beams outside the canvas
; – move the rockets, remove rockets outside the canvas
; – deal with damage to the obstacles, remove destroyed obstacles
; – check the current lives left of the tank
; – calculate the current points
; – stop the game when, yeah, when exactly?

; WorldState -> WorldState
; For each clock tick change the state of the world,
; i.e. move the space ships, the tank, the rockets and
; lasers and deal with laser and rocket hits.
(define (tock ws)
  (make-ws
   (move-tank (ws-tank ws))
   (move-rockets (ws-lor ws) (ws-ssm ws) (ws-loo ws))
   (ws-ssm ws)
   (ws-lol ws)
   (ws-loo ws)
   (ws-points ws)))

; Tank Number -> Tank
; Calculate the next position of the tank
; from its current position and its velocity.
(define (move-tank t)
  (cond
    [(< (+ (tank-xpos t) (tank-vel t))
        (* 1/2 (image-width TANK)))
     (make-tank
      (* 1/2 (image-width TANK))
      (tank-vel t)
      (tank-lives t))]
    [(> (+ (tank-xpos t) (tank-vel t))
        (- WIDTH (* 1/2 (image-width TANK))))
     (make-tank
      (- WIDTH (* 1/2 (image-width TANK)))
      (tank-vel t)
      (tank-lives t))]
    [else
     (make-tank
      (+ (tank-xpos t) (tank-vel t))
      (tank-vel t)
      (tank-lives t))]))

(check-expect (move-tank (make-tank (* 1/2 (image-width TANK))
                                    (* -1 TANK-SPEED)
                                    3))
              (make-tank (* 1/2 (image-width TANK))
                         (* -1 TANK-SPEED)
                         3))

(check-expect (move-tank (make-tank (- WIDTH (* 1/2 (image-width TANK)))
                                    (abs TANK-SPEED)
                                    3))
              (make-tank (- WIDTH (* 1/2 (image-width TANK)))
                         (abs TANK-SPEED)
                         3))

(check-expect (move-tank (make-tank (* 1/2 WIDTH)
                                    (abs TANK-SPEED)
                                    3))
              (make-tank (+ (* 1/2 WIDTH) (abs TANK-SPEED))
                         (abs TANK-SPEED)
                         3))

; LoR SSM LoO -> LoR
; Move the rockets and remove any
; outside the canvas or ones that
; have hit either an obstacle or a
; space ship.
(define (move-rockets lor ssm loo)
  (cond
    [(empty? lor) lor]
    [(cons? lor)
     (if (or (outside-canvas? (first lor))
             (hit-ship? (first lor) ssm)
             (hit-obstacles? (first lor) loo))
         (move-rockets (rest lor) ssm loo)
         (cons
          (move-rocket (first lor))
          (move-rockets (rest lor) ssm loo)))]))

(check-expect (move-rockets '() '() '())
              '())
(check-expect (move-rockets
               (list
                (make-posn 100 100)
                (make-posn 200 200)
                (make-posn 300 300)
                (make-posn 400 (+ 1 (* -1/2 (image-height ROCKET)))))
               '() '())
              (list
               (make-posn 100 (- 100 ROCKET-SPEED))
               (make-posn 200 (- 200 ROCKET-SPEED))
               (make-posn 300 (- 300 ROCKET-SPEED))))

; Rocket -> Rocket
; Move the rocket r.
(define (move-rocket r)
  (make-posn (posn-x r)
             (- (posn-y r) ROCKET-SPEED)))

(check-expect (move-rocket (make-posn 400 400))
              (make-posn 400 (- 400 ROCKET-SPEED)))

; Rocket -> Boolean
; Is the rocket r outside the canvas
; in y-direction?
(define (outside-canvas? r)
  (< (- (posn-y r) ROCKET-SPEED)
     (* -1/2 (image-height ROCKET))))

(check-expect (outside-canvas? (make-posn 100 100))
              #false)
(check-expect (outside-canvas?
               (make-posn 200
                          (- (* -1/2 (image-height ROCKET)) 1)))
              #true)

; Rocket LoO -> Boolean
; Has the rocket r hit any of the space ships in ssm?
(define (hit-ship? r ssm)
  #false)

; Rocket LoO -> Boolean
; Has the rocket r hit any of the obstacles in loo?
(define (hit-obstacles? r loo)
  (cond
    [(empty? loo) #false]
    [else
     (or (hit-obstacle? r (first loo))
         (hit-obstacles? r (rest loo)))]))

(check-expect (hit-obstacles? (make-posn 500 500)
                              (ws-loo WS4-EXAMPLE))
              #false)
(check-expect (hit-obstacles? (make-posn (* 1/2 WIDTH) OBSTACLE-Y)
                              (ws-loo WS4-EXAMPLE))
              #true)

; Rocket Obstacle -> Boolean
; Has rocket r hit obstacle o?
(define (hit-obstacle? r o)
  (and
   (and (> (posn-x r)
           (- (obstacle-xpos o) (* 1/2 (image-width OBSTACLE))))
        (< (posn-x r)
           (+ (obstacle-xpos o) (* 1/2 (image-width OBSTACLE)))))
   (and (> (posn-y r)
           (- OBSTACLE-Y (* 1/2 (image-height OBSTACLE))))
        (< (posn-y r)
           (+ (- OBSTACLE-Y (* 1/2 (image-height OBSTACLE)))
              (obstacle-height o))))))

(check-expect (hit-obstacle? (make-posn 500 500)
                             (make-obstacle
                              (+ OBSTACLE-X-FIRST
                                 (* 2 OBSTACLE-X-NEXT))
                              OBSTACLE-HEIGHT))
              #false)
(check-expect (hit-obstacle? (make-posn (* 1/2 WIDTH) OBSTACLE-Y)
                             (make-obstacle
                              (+ OBSTACLE-X-FIRST
                                 (* 2 OBSTACLE-X-NEXT))
                              OBSTACLE-HEIGHT))
              #true)

; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;BACKGROUND
;TANK
;SPACESHIP
;ROCKET
;LASER-BEAM
;OBSTACLE