;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex56) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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
     (place-rocket (- x CENTER))]))

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
 (place-image ROCKET 10 (- 53 CENTER) BACKG))

(check-expect
 (show HEIGHT)
 (place-image ROCKET 10 (- HEIGHT CENTER) BACKG))

(check-expect (show 0)
              (place-image ROCKET 10 (- 0 CENTER) BACKG))

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
    [(<= -3 x -1) (if (= x -1) HEIGHT (+ x 1))]
    [(>= x 0) (- x YDELTA)]))

(check-expect (fly "resting") "resting")
(check-expect (fly -3) -2)
(check-expect (fly -2) -1)
(check-expect (fly -1) HEIGHT)
(check-expect (fly 10) (- 10 YDELTA))
(check-expect (fly 22) (- 22 YDELTA))

; NOTE - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; This will only work when x actually reaches 0, i.e. for certain combinations
; of HEIGHT, YDELTA and CENTER (which is a calculated value).
; Currently it will not work when the program is started with (main2 100)
; or any other number that is not divisible by 3.
; END NOTE - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; LRCD -> Boolean
; Stops the program as soon as the rocket is
; out of sight.
(define (out-of-sight? x)
  (cond
    [(number? x)
     (zero? x)]
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
              #true)

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; If you watch the entire launch, you will notice that once the rocket reaches
; the top something curious happens. Explain.

; When the rocket starts rising, x eventually takes on a value
; where -3 <= x <= -1 for some YDELTA. Therefore the first conditional clause
; in show becomes true and the whole sequence starts again.