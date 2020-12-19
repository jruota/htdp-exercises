;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex258) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A Polygon is one of:
; – (list Posn Posn Posn)
; – (cons Posn Polygon)

; An NELoP is one of: 
; – (cons Posn '())
; – (cons Posn NELoP)

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; a plain background image 
(define MT (empty-scene 50 50))

(define triangle-p
  (list
    (make-posn 20 10)
    (make-posn 20 20)
    (make-posn 30 20)))

(define square-p
  (list
    (make-posn 10 10)
    (make-posn 20 10)
    (make-posn 20 20)
    (make-posn 10 20)))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; NOTE -------------------------------------------------------------------------

; One does not need to pass "connect-dots" the parameter img, since it does
; not directly change the image. Instead it call "render-line", which factually
; changes the image and passes the changed image.
; In a sense, "connect-dots" is just a wrapper to deal with empty polygons.

; END NOTE ---------------------------------------------------------------------

; Image Polygon -> Image 
; adds an image of p to MT
(define (render-poly.v2 img p)
  (local (; Image NELoP -> Image
          ; connects the Posns in p in an image
          (define (connect-dots p)
            (cond
              [(empty? (rest p)) MT]
              [else (render-line (connect-dots (rest p))
                                 (first p)
                                 (second p))]))

          ; Polygon -> Posn
          ; extracts the last item from p
          (define (last p)
            (cond
              [(empty? (rest (rest (rest p)))) (third p)]
              [else (last (rest p))]))

          (define open (connect-dots p))
          (define closed (render-line open (first p) (last p))))
    
    ; – RESULT –    
    closed))

(check-expect
  (render-poly.v2 MT triangle-p)
  (scene+line
    (scene+line
      (scene+line MT 20 10 20 20 "red")
      20 20 30 20 "red")
    30 20 20 10 "red"))

(check-expect
  (render-poly.v2 MT square-p)
  (scene+line
    (scene+line
      (scene+line
        (scene+line MT 10 10 20 10 "red")
        20 10 20 20 "red")
      20 20 10 20 "red")
    10 20 10 10 "red"))
 
; Image Posn Posn -> Image 
; draws a red line from Posn p to Posn q into im
(define (render-line im p q)
  (scene+line
    im (posn-x p) (posn-y p) (posn-x q) (posn-y q) "red"))

(check-expect (render-line MT (make-posn 10 25) (make-posn 40 25))
              (scene+line
               MT
               10
               25
               40
               25
               "red"))
