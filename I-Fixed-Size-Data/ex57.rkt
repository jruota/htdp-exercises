;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex57) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; An LRCD (for launching rocket countdown) is one of:
; – "resting"
; – a Number between -3 and -1
; – a NonnegativeNumber 
; interpretation a grounded rocket, in countdown mode,
; a number denotes the number of pixels between the
; top of the canvas and the rocket (its height)

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define HEIGHT 300) ; distances in pixels 
(define WIDTH  100)
(define YDELTA 3)
 
(define BACKG  (empty-scene WIDTH HEIGHT))
(define ROCKET (rectangle 5 30 "solid" "red"))
 
(define CENTER (/ (image-height ROCKET) 2))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; NOTE - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

; Recall that the word “height” forced us to choose one of two possible
; interpretations. Now that you have solved the exercises in this section,
; solve them again using the first interpretation of the word. Compare
; and contrast the solutions.

; The following changes will have to be introduced:
; – for fly
;   – after the countdown, set the world state to 0
;   – instead of subtracting YDELTA from x, add YDELTA to x
; – in show, the offset has to be changed to (- HEIGHT CENTER) and
;   for x values other than 0 to (- HEIGHT CENTER x)
; – the condition to stop the program has to be changed to
;   (>= x HEIGHT)
;
; While introducing these changes are not according to the conventional
; way of dealing with images, where height is the distance between the top
; of the canvas and the reference point, they make defining the stop-when
; function much easier and more stable.

; END NOTE - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

; LRCD -> LRCD
; Start the rocket launching program.
(define (main2 s)
  (big-bang s
    [to-draw show]
    [on-key launch]
    [on-tick fly 1]
    [stop-when out-of-sight? show]))    ; see note for out-of-sight?

; LRCD -> Image
; renders the state as a resting or flying rocket 
(define (show x)
  (cond
    [(string? x)
     (place-rocket (- HEIGHT CENTER))]
    [(<= -3 x -1)
     (place-image (text (number->string x) 20 "red")
                  10
                  (* 3/4 WIDTH)
                  (place-rocket (- HEIGHT CENTER)))]
    [(>= x 0)
     (place-rocket (- HEIGHT CENTER x))]))

(check-expect
 (show "resting")
 (place-image ROCKET 10 (- HEIGHT CENTER) BACKG))

(check-expect
 (show -2)
 (place-image (text "-2" 20 "red")
              10 (* 3/4 WIDTH)
              (place-image ROCKET 10 (- HEIGHT CENTER) BACKG)))
 
(check-expect
 (show 53)
 (place-image ROCKET 10 (- HEIGHT CENTER 53) BACKG))

(check-expect
 (show HEIGHT)
 (place-image ROCKET 10 (- HEIGHT CENTER HEIGHT) BACKG))

(check-expect (show 0)
              (place-image ROCKET 10 (- HEIGHT CENTER) BACKG))

; Number -> Image
; Place the rocket at y in BACKG.
(define (place-rocket y)
  (place-image ROCKET 10 y BACKG))

(check-expect (place-rocket 100)
              (place-image ROCKET 10 100 BACKG))
 
; LRCD KeyEvent -> LRCD
; starts the countdown when space bar is pressed, 
; if the rocket is still resting 
(define (launch x ke)
  (cond
    [(string? x) (if (string=? " " ke) -3 x)]
    [(<= -3 x -1) x]
    [(>= x 0) x]))

(check-expect (launch "resting" " ") -3)
(check-expect (launch "resting" "a") "resting")
(check-expect (launch -3 " ") -3)
(check-expect (launch -1 " ") -1)
(check-expect (launch 33 " ") 33)
(check-expect (launch 33 "a") 33)
 
; LRCD -> LRCD
; raises the rocket by YDELTA,
; if it is moving already 
(define (fly x)
  (cond
    [(string? x) x]
    [(<= -3 x -1) (if (= x -1) 0 (+ x 1))]
    [(>= x 0) (+ x YDELTA)]))

(check-expect (fly "resting") "resting")
(check-expect (fly -3) -2)
(check-expect (fly -2) -1)
(check-expect (fly -1) 0)
(check-expect (fly 10) (+ 10 YDELTA))
(check-expect (fly 22) (+ 22 YDELTA))

; LRCD -> Boolean
; Stops the program as soon as the rocket is
; out of sight.
(define (out-of-sight? x)
  (cond
    [(number? x)
     (>= x HEIGHT)]
    [else
     #false]))

(check-expect (out-of-sight? "resting")
              #false)
(check-expect (out-of-sight? -1)
              #false)
(check-expect (out-of-sight? -2)
              #false)
(check-expect (out-of-sight? -3)
              #false)
(check-expect (out-of-sight? 1)
              #false)
(check-expect (out-of-sight? 0)
              #false)
(check-expect (out-of-sight? (- HEIGHT 1))
              #false)
(check-expect (out-of-sight? HEIGHT)
              #true)
(check-expect (out-of-sight? (+ HEIGHT 1))
              #true)

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; If you watch the entire launch, you will notice that once the rocket reaches
; the top something curious happens. Explain.

; When the rocket starts rising, x eventually takes on a value
; where -3 <= x <= -1 for some YDELTA. Therefore the first conditional clause
; in show becomes true and the whole sequence starts again.