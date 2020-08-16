;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex193-2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; An NELoP is one of: 
; – (cons Posn '())
; – (cons Posn NELoP)
; Interpretation:
;     A non-empty list of points.

; a Polygon is one of: 
; – (cons Posn (cons Posn (cons Posn '()))) 
; – (cons Posn Polygon)
; Interpretation:
;     A list of coordinates representing the
;     points of a polygon.
; Constraints:
;     For polygons of three points, those points
;     may not lie on a single line.

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; a plain background image 
(define MT (empty-scene 50 50))

(define triangle-p (list (make-posn 20 10)
                         (make-posn 20 20)
                         (make-posn 30 20)))

(define square-p (list (make-posn 10 10)
                       (make-posn 20 10)
                       (make-posn 20 20)
                       (make-posn 10 20)))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Image Polygon -> Image 
; adds an image of p to img
(define (render-poly img p)
  (connect-dots img (add-at-end p (first p))))

(check-expect (render-poly MT triangle-p)
              (scene+line
               (scene+line
                (scene+line MT 20 10 20 20 "red")
                20 20 30 20 "red")
               30 20 20 10 "red"))

(check-expect (render-poly MT square-p)
              (scene+line
               (scene+line
                (scene+line
                 (scene+line MT 10 10 20 10 "red")
                 20 10 20 20 "red")
                20 20 10 20 "red")
               10 20 10 10 "red"))

; Image NELoP -> Image 
; connects the dots in p by rendering lines in img
(define (connect-dots img p)
  (cond
    [(empty? (rest p)) img]
    [else
     (render-line
       (connect-dots img (rest p))
       (first p)
       (second p))]))

(check-expect (connect-dots MT triangle-p)
              (scene+line
               (scene+line MT 20 20 30 20 "red")
               20 10 20 20 "red"))

(check-expect (connect-dots MT square-p)
              (scene+line
               (scene+line
                (scene+line MT 10 10 20 10 "red")
                20 10 20 20 "red")
               20 20 10 20 "red"))

; Image Posn Posn -> Image 
; draws a red line from Posn p to Posn q into im
(define (render-line img p q)
  (scene+line
   img
   (posn-x p) (posn-y p) (posn-x q) (posn-y q)
   "red"))

(check-expect (render-line MT (make-posn 10 25) (make-posn 40 25))
              (scene+line
               MT
               10
               25
               40
               25
               "red"))

; Polygon -> Posn
; extracts the last item from p
(define (last p)
  (cond
    [(empty? (rest (rest (rest p)))) (third p)]
    [else (last (rest p))]))

(check-expect (last (cons (make-posn 10 10)
                          (cons (make-posn 20 20)
                                (cons (make-posn 30 30) '()))))
              (make-posn 30 30))
(check-expect (last (cons (make-posn 10 10)
                          (cons (make-posn 20 20)
                                (cons (make-posn 30 30)
                                      (cons (make-posn 40 40)
                                            (cons (make-posn 50 50) '()))))))
              (make-posn 50 50))

; Polygon Posn -> Polygon
; Add pos at the end of pol.
(define (add-at-end pol pos)
  (cond
    [(empty? (rest (rest (rest pol))))
     (list (first pol)
           (second pol)
           (third pol)
           pos)]
    [else
     (cons (first pol)
           (add-at-end (rest pol) pos))]))

(check-expect (add-at-end (list (make-posn 10 10)
                                (make-posn 20 20)
                                (make-posn 30 30))
                          (make-posn 40 40))
              (list (make-posn 10 10)
                    (make-posn 20 20)
                    (make-posn 30 30)
                    (make-posn 40 40)))
(check-expect (add-at-end (list (make-posn 10 10)
                                (make-posn 20 20)
                                (make-posn 30 30)
                                (make-posn 40 40)
                                (make-posn 50 50))
                          (make-posn 60 60))
              (list (make-posn 10 10)
                    (make-posn 20 20)
                    (make-posn 30 30)
                    (make-posn 40 40)
                    (make-posn 50 50)
                    (make-posn 60 60)))