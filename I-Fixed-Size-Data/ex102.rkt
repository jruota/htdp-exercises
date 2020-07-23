;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex102) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

; A MissileOrNot is one of: 
; – #false
; – Posn
; interpretation#false means the missile is in the tank;
; Posn says the missile is at that location

(define-struct sigs [ufo tank missile])
; A SIGS (short for space invader space game) is a structure:
;   (make-sigs UFO Tank MissileOrNot)
; interpretation represents the complete state of a
; space invader game

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

; for testing
(define SCENE (place-image UFO
                           (* 1/2 WIDTH)
                           (* 2 (image-height UFO))
                           (place-image TANK
                                        32
                                        TANK-Y
                                        BACKGROUND)))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Any -> Image
; The only argument is discarded.
(define (si-main a)
  (big-bang (make-sigs (make-posn (* 1/2 WIDTH) (* -1 (image-height UFO)))
                       (make-tank (* 1/2 WIDTH) TANK-SPEED)
                       #false)
    [on-draw si-render]
    [on-tick si-move]
    [on-key si-control]
    [stop-when si-game-over? si-render-final]))

; SIGS -> Image
; Render the current state of the game.
(define (si-render s)
  (missile-render (sigs-missile s)
                  (ufo-render (sigs-ufo s)
                              (tank-render (sigs-tank s)
                                           BACKGROUND))))

(check-expect (si-render (make-sigs (make-posn 20 10)
                                    (make-tank 28 -3)
                                    #false))
              (place-image
               UFO
               20
               10
               (place-image
                TANK
                28
                TANK-Y
                BACKGROUND)))

(check-expect (si-render (make-sigs (make-posn 20 10)
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

(check-expect (si-render (make-sigs (make-posn 20 100)
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

; SIGS KeyEvent -> SIGS
; Change the movement direction of the tank
; and fire the missile when not already fired.
(define (si-control s ke)
  (cond
    [(key=? ke "left")
     (make-sigs (sigs-ufo s)
                (make-tank (tank-loc (sigs-tank s))
                           (* -1 (abs (tank-vel (sigs-tank s)))))
                (sigs-missile s))]
    [(key=? ke "right")
     (make-sigs (sigs-ufo s)
                (make-tank (tank-loc (sigs-tank s))
                           (abs (tank-vel (sigs-tank s))))
                (sigs-missile s))]
    [(and (key=? ke " ") (false? (sigs-missile s)))
     (make-sigs (sigs-ufo s)
                (sigs-tank s)
                (make-posn (tank-loc (sigs-tank s))
                           TANK-Y))]
    [else s]))
    

(check-expect (si-control (make-sigs (make-posn 10 10)
                                     (make-tank 10 3)
                                     #false)
                          "left")
              (make-sigs (make-posn 10 10)
                         (make-tank 10 -3)
                         #false))

(check-expect (si-control (make-sigs (make-posn 10 10)
                                     (make-tank 10 -3)
                                     (make-posn 50 50))
                          "right")
              (make-sigs (make-posn 10 10)
                         (make-tank 10 3)
                         (make-posn 50 50)))

(check-expect (si-control (make-sigs (make-posn 10 10)
                                     (make-tank 10 3)
                                     #false)
                          " ")
              (make-sigs (make-posn 10 10)
                         (make-tank 10 3)
                         (make-posn 10 TANK-Y)))

(check-expect (si-control (make-sigs (make-posn 10 10)
                                     (make-tank 10 3)
                                     (make-posn 50 50))
                          " ")
              (make-sigs (make-posn 10 10)
                         (make-tank 10 3)
                         (make-posn 50 50)))

(check-expect (si-control (make-sigs (make-posn 10 10)
                                     (make-tank 10 3)
                                     (make-posn 50 50))
                          "a")
              (make-sigs (make-posn 10 10)
                         (make-tank 10 3)
                         (make-posn 50 50)))


; SIGS -> SIGS
; Determine the next position of the UFO,
; the tank and the missile, if present.
(define (si-move s)
  (make-sigs (move-ufo (sigs-ufo s) (random-jump UFO-JUMP))
             (move-tank (sigs-tank s))
             (move-missile (sigs-missile s))))

; SIGS -> Boolean
; Check whether the UFO has landed or
; been hit by the missile.
(define (si-game-over? s)
  (or (ufo-landed? (sigs-ufo s))
      (ufo-hit? (sigs-ufo s) (sigs-missile s))))

; not hit, not landed, no missile fired
(check-expect (si-game-over? (make-sigs
                              (make-posn (* 1/2 WIDTH) (* 1/2 HEIGHT))
                              (make-tank (* 1/2 WIDTH) 3)
                              #false))
              #false)
; not hit, not landed, missile fired
(check-expect (si-game-over? (make-sigs
                              (make-posn (* 1/2 WIDTH) (* 1/2 HEIGHT))
                              (make-tank (* 1/2 WIDTH) 3)
                              (make-posn (* 1/2 WIDTH) TANK-Y)))
              #false)
; landed
(check-expect (si-game-over? (make-sigs
                              (make-posn (* 1/2 WIDTH)
                                         (- HEIGHT
                                            (image-height GROUND)
                                            (* 1/2 (image-height UFO))))
                              (make-tank (* 1/2 WIDTH) TANK-SPEED)
                              #false))
              #true)
; hit
(check-expect (si-game-over? (make-sigs
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

(check-expect (si-render-final (make-sigs
                                (make-posn (* 1/2 WIDTH) (* 1/2 HEIGHT))
                                (make-tank (* 1/2 WIDTH) TANK-SPEED)
                                #false))
              (place-image (text "GAME OVER" TEXT-SIZE "red")
                           (* 1/2 WIDTH)
                           (* 1/2 HEIGHT)
                           (si-render (make-sigs
                                       (make-posn (* 1/2 WIDTH) (* 1/2 HEIGHT))
                                       (make-tank (* 1/2 WIDTH) TANK-SPEED)
                                       #false))))

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
  (cond
    [(false? m) #false]
    [(posn? m)
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
               (* 1/2 UFO-WING-HEIGHT))))]))

; no missile fired
(check-expect (ufo-hit? (make-posn 100 100) #false)
              #false)
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

; Posn -> Image
; Add UFO to scene s.
(define (ufo-render u s)
  (place-image UFO (posn-x u) (posn-y u) s))

(check-expect (ufo-render (make-posn (* 1/2 WIDTH) (* 1/2 HEIGHT)) BACKGROUND)
              (place-image UFO
                           (* 1/2 WIDTH)
                           (* 1/2 HEIGHT)
                           BACKGROUND))

; Tank -> Image
; Add TANK to scene s.
(define (tank-render t s)
  (place-image TANK (tank-loc t) TANK-Y s))

(check-expect (tank-render (make-tank (* 1/2 WIDTH) TANK-SPEED) BACKGROUND)
              (place-image TANK
                           (* 1/2 WIDTH)
                           TANK-Y
                           BACKGROUND))

; MissileOrNot Image -> Image 
; adds an image of missile m to scene s 
(define (missile-render m s)
  (cond
    [(boolean? m) s]
    [(posn? m)
     (place-image MISSILE (posn-x m) (posn-y m) s)]))

(check-expect (missile-render #false SCENE)
              SCENE)
(check-expect (missile-render (make-posn 32 (- HEIGHT TANK-HEIGHT 10))
                                 SCENE)
              (place-image MISSILE
                           32
                           (- HEIGHT TANK-HEIGHT 10)
                           SCENE))

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
  (cond
    [(false? m) #false]
    [(posn? m)
     (make-posn (posn-x m) (- (posn-y m) MISSILE-SPEED))]))

(check-expect (move-missile #false) #false)
(check-expect (move-missile (make-posn (* 1/2 WIDTH) (* 1/2 HEIGHT)))
              (make-posn (* 1/2 WIDTH) (- (* 1/2 HEIGHT) MISSILE-SPEED)))

; Number -> Number
; Calculate a random jump in x-direction
; for the UFO, where the jump is within [-x/2, x/2].
(define (random-jump x)
  (- (random x) (floor (* 1/2 x))))

(check-within (random-jump 11) 0 5)