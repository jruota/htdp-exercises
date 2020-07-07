;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex47) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Happiness is a Number.
; Interpretation:
;     The current happiness value.

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; graphical constants

(define WIDTH 500)
(define HEIGHT 20)
(define SCENE (empty-scene WIDTH HEIGHT))    ; already has the black outline

; other constants
(define HPNS-DCRS (/ WIDTH -1000))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Happiness -> Image
; Starts the program.
; The passed value should be less than or equal
; to WIDTH, i.e. hap <= WIDTH.
(define (gauge-prog hap)
  (big-bang hap
    [on-draw render]
    [on-tick tock]
    [on-key ke-handler]))

; Happiness -> Image
; Render an image of the current world state.
(define (render hap)
  (place-image/align (rectangle hap HEIGHT "solid" "red")
                     0
                     0
                     "left"
                     "top"
                     SCENE))

(check-expect (render 0)
              (place-image/align (rectangle 0 HEIGHT "solid" "red")
                                 0
                                 0
                                 "left"
                                 "top"
                                 SCENE))
(check-expect (render WIDTH)
              (place-image/align (rectangle WIDTH HEIGHT "solid" "red")
                                 0
                                 0
                                 "left"
                                 "top"
                                 SCENE))
(check-expect (render (/ WIDTH 2))
              (place-image/align (rectangle (/ WIDTH 2) HEIGHT "solid" "red")
                                 0
                                 0
                                 "left"
                                 "top"
                                 SCENE))

; Happiness -> Happiness
; Decrease the happiness by HPNS-DCRS per clock tick.
(define (tock hap)
  (max 0 (+ hap HPNS-DCRS)))

(check-expect (tock 100)
              (+ 100 HPNS-DCRS))
(check-expect (tock -99)
              0)

; Happiness KeyEvent -> Happiness
; Decrease happines by one fifth when the down arrow is pressed,
; increase happiness by one third when the up arrow is pressed.
(define (ke-handler hap ke)
  (cond
    [(string=? ke "down")
     (max 0 (* hap 4/5))]
    [(string=? ke "up")
     (min WIDTH (* hap 4/3))]
    [else hap]))

(check-expect (ke-handler 100 "down")
              (* 100 4/5))
(check-expect (ke-handler 0 "down")
              0)
(check-expect (ke-handler WIDTH "up")
              WIDTH)
(check-expect (ke-handler (/ WIDTH 2) "up")
              (* (/ WIDTH 2) 4/3))
(check-expect (ke-handler 0 "up")
              0)
(check-expect (ke-handler 50 "a")
              50)