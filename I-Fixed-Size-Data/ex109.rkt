;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex109) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define AA "start, expect an 'a'")
(define BB "expect 'b', 'c', or 'd'")
(define DD "finished")
(define ER "error, illegal key")

; ExpectsToSee is one of:
; – AA
; – BB
; – DD 
; – ER 

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define WIDTH 100)
(define HEIGHT WIDTH)

(define WHITE (empty-scene WIDTH HEIGHT))
(define YELLOW (empty-scene WIDTH HEIGHT "yellow"))
(define GREEN (empty-scene WIDTH HEIGHT "green"))
(define RED (empty-scene WIDTH HEIGHT "red"))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Any -> Image
; Start the main program.
; The only argument will be ignored.
(define (main a)
  (big-bang AA
    [on-draw render]
    [on-key ke-handler]
    [stop-when over? render]))

; ExpectsToSee -> Image
; Render the current state of the world
(define (render ets)
  (cond
    [(equal? AA ets) WHITE]
    [(equal? BB ets) YELLOW]
    [(equal? DD ets) GREEN]
    [(equal? ER ets) RED]))

(check-expect (render AA)
              WHITE)
(check-expect (render BB)
              YELLOW)
(check-expect (render DD)
              GREEN)
(check-expect (render ER)
              RED)

; ExpectsToSee -> ExpectsToSee
; Change the state of the world according
; to the key pressed.
(define (ke-handler ets ke)
  (cond
    [(equal? AA ets)
     (cond
       [(key=? ke "a") BB]
       [else ER])]
    [(equal? BB ets)
     (cond
       [(or (key=? ke "b") (key=? ke "c"))
        BB]
       [(key=? ke "d")
        DD]
       [else ER])]
    [else ER]))

(check-expect (ke-handler AA "a")
              BB)
(check-expect (ke-handler AA "b")
              ER)
(check-expect (ke-handler AA "c")
              ER)
(check-expect (ke-handler AA "d")
              ER)
(check-expect (ke-handler AA " ")
              ER)

(check-expect (ke-handler BB "b")
              BB)
(check-expect (ke-handler BB "c")
              BB)
(check-expect (ke-handler BB "d")
              DD)
(check-expect (ke-handler BB "a")
              ER)

; ExpectsToSee -> Boolean
; Check whether the game is over.
(define (over? ets)
  (or (equal? ER ets)
      (equal? DD ets)))

(check-expect (over? AA)
              #false)
(check-expect (over? BB)
              #false)
(check-expect (over? DD)
              #true)
(check-expect (over? ER)
              #true)