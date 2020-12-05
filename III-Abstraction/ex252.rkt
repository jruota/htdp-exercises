;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex252) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; graphical constants:    
(define emt
  (empty-scene 100 100))
(define dot
  (circle 3 "solid" "red"))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; [List-of Number] -> Number
(define (product l)
  (cond
    [(empty? l) 1]
    [else
     (* (first l)
        (product
          (rest l)))]))

(check-expect (product (list))
              1)
(check-expect (product (list 1 2 3 4 5 6 7 8 9))
              362880)

; [List-of Posn] -> Image
(define (image* l)
  (cond
    [(empty? l) emt]
    [else
     (place-dot
      (first l)
      (image* (rest l)))]))

(check-expect (image* empty)
              emt)
(check-expect (image* (list (make-posn 0 0)
                            (make-posn 10 10)
                            (make-posn 20 20)
                            (make-posn 30 30)
                            (make-posn 40 40)
                            (make-posn 50 50)
                            (make-posn 60 60)
                            (make-posn 70 70)
                            (make-posn 80 80)
                            (make-posn 90 90)
                            (make-posn 100 100)))
              (place-image
               dot 0 0
               (place-image
                dot 10 10
                (place-image
                 dot 20 20
                 (place-image
                  dot 30 30
                  (place-image
                   dot 40 40
                   (place-image
                    dot 50 50
                    (place-image
                     dot 60 60
                     (place-image
                      dot 70 70
                      (place-image
                       dot 80 80
                       (place-image
                        dot 90 90
                        (place-image
                         dot 100 100 emt))))))))))))
 
; Posn Image -> Image 
(define (place-dot p img)
  (place-image
     dot
     (posn-x p) (posn-y p)
     img))

(check-expect (place-dot (make-posn 50 50) emt)
              (place-image dot 50 50 emt))

; [List-of X] Y [X Y -> Y] -> Y
; Compute a value by applying
; f to every element in l.
; Return ev if l is empty.
(define (fold2 l ev f)
  (cond
    [(empty? l)
     ev]
    [else
     (f (first l)
        (fold2 (rest l) ev f))]))

(check-expect (fold2 empty 1 *)
              1)
(check-expect (fold2 (list 1 2 3 4 5 6 7 8 9) 1 *)
              362880)

(check-expect (fold2 empty emt place-dot)
              emt)
(check-expect (fold2 (list (make-posn 0 0)
                           (make-posn 10 10)
                           (make-posn 20 20)
                           (make-posn 30 30)
                           (make-posn 40 40)
                           (make-posn 50 50)
                           (make-posn 60 60)
                           (make-posn 70 70)
                           (make-posn 80 80)
                           (make-posn 90 90)
                           (make-posn 100 100))
                     emt
                     place-dot)
              (place-image
               dot 0 0
               (place-image
                dot 10 10
                (place-image
                 dot 20 20
                 (place-image
                  dot 30 30
                  (place-image
                   dot 40 40
                   (place-image
                    dot 50 50
                    (place-image
                     dot 60 60
                     (place-image
                      dot 70 70
                      (place-image
                       dot 80 80
                       (place-image
                        dot 90 90
                        (place-image
                         dot 100 100 emt))))))))))))
