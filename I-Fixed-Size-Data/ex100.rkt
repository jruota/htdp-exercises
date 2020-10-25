;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex100) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A UFO is a Posn. 
; interpretation (make-posn x y) is the UFO's location 
; (using the top-down, left-to-right convention)
 
(define-struct tank [loc vel])
; A Tank is a structure:
;   (make-tank Number Number). 
; interpretation (make-tank x dx) specifies the position:
; (x, HEIGHT) and the tank's speed: dx pixels/tick 
 
; A Missile is a Posn. 
; interpretation (make-posn x y) is the missile's place

(define-struct aim [ufo tank])
(define-struct fired [ufo tank missile])

; A SIGS (space invade game state) is one of: 
; – (make-aim UFO Tank)
; – (make-fired UFO Tank Missile)
; interpretation represents the complete state of a 
; space invader game

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; colors can be found at
; https://docs.racket-lang.org/draw/color-database___.html

; graphical constants
(define WIDTH 400)    ; -> ENTRY POINT
(define HEIGHT (* 3/2 WIDTH))

(define GROUND (rectangle WIDTH (* 2/50 WIDTH) "solid" "SeaGreen"))

(define BACKGROUND (overlay/align
                    "middle"
                    "bottom"
                    GROUND
                    (empty-scene WIDTH HEIGHT "DarkSlateGray")))

(define TANK (rectangle (* 1/6 WIDTH) (* 2/50 WIDTH) "solid" "olive"))
(define TANK-Y (- HEIGHT (+ (* 1/2 (image-height TANK))
                            (image-height GROUND))))
(define TANK-HEIGHT (image-height TANK))
(define MISSILE (triangle (* 3/100 WIDTH) "solid" "red"))
(define UFO-RADIUS (* 1/25 WIDTH))
(define UFO-WING-HEIGHT (* 1/50 WIDTH))
(define UFO-WING-SPAN (* 1/6 WIDTH))
(define UFO (overlay (circle UFO-RADIUS "solid" "green")
                     (rectangle UFO-WING-SPAN UFO-WING-HEIGHT "solid" "green")))

(define TEXT-SIZE (* 1/10 WIDTH))

; physical constants
(define UFO-SPEED 3)
(define UFO-JUMP 13)    ; make this a positive, uneven number
(define MISSILE-SPEED (* 2 UFO-SPEED))
(define TANK-SPEED UFO-SPEED)

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Any -> SIGS
; The only argument is discarded.
(define (si-main a)
  (big-bang (make-aim (make-posn (* 1/2 WIDTH) (* -1 (image-height UFO)))
                      (make-tank (* 1/2 WIDTH) TANK-SPEED))
    [on-draw si-render]
    [on-tick si-move]
    [on-key si-control]
    [stop-when si-game-over? si-render-final]))

; SIGS -> Image
; renders the given game state on top of BACKGROUND 
; for examples see figure 32
(define (si-render s)
  (cond
    [(aim? s)
     (ufo-render (aim-ufo s)
                  (tank-render (aim-tank s) BACKGROUND))]
    [(fired? s)
     (missile-render
       (fired-missile s)
       (ufo-render (fired-ufo s)
                   (tank-render (fired-tank s)
                                BACKGROUND)))]))

(check-expect (si-render (make-aim (make-posn 20 10) (make-tank 28 -3)))
              (place-image
               UFO
               20
               10
               (place-image
                TANK
                28
                TANK-Y
                BACKGROUND)))

(check-expect (si-render (make-fired (make-posn 20 10)
                                     (make-tank 28 -3)
                                     (make-posn 28 (- HEIGHT TANK-HEIGHT))))
              (place-image
               UFO
               20
               10
               (place-image
                MISSILE
                28
                (- HEIGHT TANK-HEIGHT)
                (place-image
                 TANK
                 28
                 TANK-Y
                 BACKGROUND))))

(check-expect (si-render (make-fired (make-posn 20 100)
                                     (make-tank 100 3)
                                     (make-posn 22 103)))
              (place-image
               MISSILE
               22
               103
               (place-image
                UFO
                20
                100
                (place-image
                 TANK
                 100
                 TANK-Y
                 BACKGROUND))))

; SIGS -> SIGS
; Determine the next position of the UFO,
; the tank and the missile, if present.
(define (si-move s)
  (cond
    [(aim? s) (make-aim (move-ufo (aim-ufo s) (random-jump UFO-JUMP))
                        (move-tank (aim-tank s)))]
    [(fired? s) (make-fired (move-ufo (fired-ufo s) (random-jump UFO-JUMP))
                            (move-tank (fired-tank s))
                            (move-missile (fired-missile s)))]))

; SIGS KeyEvent -> SIGS
; Change the movement direction of the tank
; and fire the missile when not already fired.
(define (si-control s ke)
  (cond
    [(aim? s)
     (cond
       [(key=? ke "left")
        (make-aim (aim-ufo s)
                  (make-tank (tank-loc (aim-tank s))
                             (* -1 (abs (tank-vel (aim-tank s))))))]
       [(key=? ke "right")
        (make-aim (aim-ufo s)
                  (make-tank (tank-loc (aim-tank s))
                             (abs (tank-vel (aim-tank s)))))]
       [(key=? ke " ")
        (make-fired (aim-ufo s)
                    (aim-tank s)
                    (make-posn (tank-loc (aim-tank s))
                               TANK-Y))])]
    [(fired? s)
     (cond
       [(key=? ke "left")
        (make-fired (fired-ufo s)
                    (make-tank (tank-loc (fired-tank s))
                               (* -1 (abs (tank-vel (fired-tank s)))))
                    (fired-missile s))]
       [(key=? ke "right")
        (make-fired (fired-ufo s)
                    (make-tank (tank-loc (fired-tank s))
                               (abs (tank-vel (fired-tank s))))
                    (fired-missile s))]
       [else s])]))

(check-expect (si-control (make-aim (make-posn 10 10)
                                    (make-tank 10 3))
                          "left")
              (make-aim (make-posn 10 10)
                        (make-tank 10 -3)))

(check-expect (si-control (make-aim (make-posn 10 10)
                                    (make-tank 10 -3))
                          "right")
              (make-aim (make-posn 10 10)
                        (make-tank 10 3)))

(check-expect (si-control (make-aim (make-posn 10 10)
                                    (make-tank 10 3))
                          " ")
              (make-fired (make-posn 10 10)
                          (make-tank 10 3)
                          (make-posn 10 TANK-Y)))

(check-expect (si-control (make-fired (make-posn 100 100)
                                      (make-tank 10 3)
                                      (make-posn 50 50))
                          "left")
              (make-fired (make-posn 100 100)
                          (make-tank 10 -3)
                          (make-posn 50 50)))

(check-expect (si-control (make-fired (make-posn 100 100)
                                      (make-tank 10 -3)
                                      (make-posn 50 50))
                          "right")
              (make-fired (make-posn 100 100)
                          (make-tank 10 3)
                          (make-posn 50 50)))

; SIGS -> Boolean
; Check whether the UFO has landed or
; been hit by the missile.
(define (si-game-over? s)
  (cond
    [(aim? s)
     (ufo-landed? (aim-ufo s))]
    [(fired? s)
     (or (ufo-landed? (fired-ufo s))
         (ufo-hit? (fired-ufo s) (fired-missile s)))]))

; not hit, not landed, no missile fired
(check-expect (si-game-over? (make-aim
                              (make-posn (* 1/2 WIDTH) (* 1/2 HEIGHT))
                              (make-tank (* 1/2 WIDTH) 3)))
              #false)
; not hit, not landed, missile fired
(check-expect (si-game-over? (make-fired
                              (make-posn (* 1/2 WIDTH) (* 1/2 HEIGHT))
                              (make-tank (* 1/2 WIDTH) 3)
                              (make-posn (* 1/2 WIDTH) TANK-Y)))
              #false)
; landed
(check-expect (si-game-over? (make-aim
                              (make-posn (* 1/2 WIDTH)
                                         (- HEIGHT
                                            (image-height GROUND)
                                            (* 1/2 (image-height UFO))))
                              (make-tank (* 1/2 WIDTH) TANK-SPEED)))
              #true)
; hit
(check-expect (si-game-over? (make-fired
                              (make-posn (* 1/2 WIDTH) (* 1/2 HEIGHT))
                              (make-tank (* 1/2 WIDTH) TANK-SPEED)
                              (make-posn (+ (* 1/2 WIDTH)
                                            (* 1/2 (image-width UFO)))
                                         (* 1/2 HEIGHT))))
              #true)

; SIGS -> Image
; Render a final image of the game,
; with GAME OVER written over the scene.
(define (si-render-final s)
  (place-image (text "GAME OVER" TEXT-SIZE "red")
               (* 1/2 WIDTH)
               (* 1/2 HEIGHT)
               (si-render s)))

(check-expect (si-render-final (make-aim
                                (make-posn (* 1/2 WIDTH) (* 1/2 HEIGHT))
                                (make-tank (* 1/2 WIDTH) TANK-SPEED)))
              (place-image (text "GAME OVER" TEXT-SIZE "red")
                           (* 1/2 WIDTH)
                           (* 1/2 HEIGHT)
                           (si-render (make-aim
                                       (make-posn (* 1/2 WIDTH) (* 1/2 HEIGHT))
                                       (make-tank (* 1/2 WIDTH) TANK-SPEED)))))

; UFO -> Boolean
; Check whether the UFO has landed
; on the ground.
(define (ufo-landed? u)
  (>= (posn-y u)
      (- HEIGHT (image-height GROUND) (* 1/2 (image-height UFO)))))

(check-expect (ufo-landed? (make-posn (* 1/2 WIDTH) (* 1/2 HEIGHT)))
              #false)
(check-expect (ufo-landed? (make-posn (* 1/2 WIDTH)
                                      (- HEIGHT
                                         (image-height GROUND)
                                         (* 1/2 (image-height UFO)))))
              #true)

; UFO MISSILE -> Boolean
; Check whether the UFO has been
; hit by MISSILE.
(define (ufo-hit? u m)
  (or
   ; hit in center
   (and (<= (abs (- (posn-x u) (posn-x m)))
            UFO-RADIUS)
        (<= (abs (- (posn-y u) (posn-y m)))
            UFO-RADIUS))
   ; hit in the wings
   (and (<= (abs (- (posn-x u) (posn-x m)))
            UFO-WING-SPAN)
        (<= (abs (- (posn-y u) (posn-y m)))
            (* 1/2 UFO-WING-HEIGHT)))))

; wing hit
(check-expect (ufo-hit? (make-posn (* 1/2 WIDTH) (* 1/2 HEIGHT))
                        (make-posn (+ (* 1/2 WIDTH)
                                      (* 1/2 (image-width UFO)))
                                   (* 1/2 HEIGHT)))
              #true)

; wing not hit yet
(check-expect (ufo-hit? (make-posn (* 1/2 WIDTH) (* 1/2 HEIGHT))
                        (make-posn (+ (* 1/2 WIDTH) (* 1/2 (image-width UFO)))
                                   (+ (* 1/2 HEIGHT) (image-height MISSILE))))
              #false)

; center hit
(check-expect (ufo-hit? (make-posn (* 1/2 WIDTH) (* 1/2 HEIGHT))
                        (make-posn (+ (* 1/2 WIDTH) (* 1/7 (image-width UFO)))
                                   (+ (* 1/2 HEIGHT) (image-height MISSILE))))
              #true)
   

; Tank Image -> Image 
; adds t to the given image im
(define (tank-render t im)
  (place-image TANK
               (tank-loc t)
               TANK-Y
               im))

(check-expect (tank-render (make-tank (* 1/2 WIDTH) TANK-SPEED) BACKGROUND)
              (place-image TANK
                           (* 1/2 WIDTH)
                           TANK-Y
                           BACKGROUND))
 
; UFO Image -> Image 
; adds u to the given image im
(define (ufo-render u im)
  (place-image UFO
               (posn-x u)
               (posn-y u)
               im))

(check-expect (ufo-render (make-posn (* 1/2 WIDTH) (* 1/2 WIDTH)) BACKGROUND)
              (place-image UFO
                           (* 1/2 WIDTH)
                           (* 1/2 WIDTH)
                           BACKGROUND))

; Missile Image -> Image 
; adds m to the given image im
(define (missile-render m im)
  (place-image MISSILE
               (posn-x m)
               (posn-y m)
               im))

(check-expect (missile-render (make-posn (* 1/2 WIDTH)
                                         (* 1/2 WIDTH))
                                         BACKGROUND)
              (place-image MISSILE
                           (* 1/2 WIDTH)
                           (* 1/2 WIDTH)
                           BACKGROUND))

; Posn -> Posn
; Move the UFO down by UFO-SPEED and
; move it by dx in the x-direction.
(define (move-ufo u dx)
  (make-posn (+ (posn-x u) dx)
             (+ (posn-y u) UFO-SPEED)))

(check-expect (move-ufo (make-posn 10 10) 3)
              (make-posn 13 (+ 10 UFO-SPEED)))

; Tank -> Tank
; Move the TANK along the x-axis.
(define (move-tank t)
  (cond
    ; left border of the scene
    [(<= (+ (tank-loc t) (tank-vel t))
         (* 1/2 (image-width TANK)))
     (make-tank (* 1/2 (image-width TANK)) (tank-vel t))]
    ; right border of the scene
    [(>= (+ (tank-loc t) (tank-vel t))
         (- WIDTH (* 1/2 (image-width TANK))))
     (make-tank (- WIDTH (* 1/2 (image-width TANK))) (tank-vel t))]
    [else
     (make-tank (+ (tank-loc t) (tank-vel t)) (tank-vel t))]))

(check-expect (move-tank (make-tank -10 3))
              (make-tank (* 1/2 (image-width TANK)) 3))
(check-expect (move-tank (make-tank (+ WIDTH 10) -3))
              (make-tank (- WIDTH (* 1/2 (image-width TANK))) -3))
(check-expect (move-tank (make-tank (* 1/2 WIDTH) 3))
              (make-tank (+ (* 1/2 WIDTH) 3) 3))

; Posn -> Posn
; Move the missile up a vertical line.
(define (move-missile m)
  (make-posn (posn-x m) (- (posn-y m) MISSILE-SPEED)))

(check-expect (move-missile (make-posn (* 1/2 WIDTH) (* 1/2 HEIGHT)))
              (make-posn (* 1/2 WIDTH) (- (* 1/2 HEIGHT) MISSILE-SPEED)))

; Number -> Number
; Calculate a random jump in x-direction
; for the UFO, where the jump is within [-x/2, x/2].
(define (random-jump x)
  (- (random x) (floor (* 1/2 x))))

(check-within (random-jump 11) 0 5)
