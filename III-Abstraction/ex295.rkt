;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex295) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; CONSTANTS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; distances in terms of pixels 
(define WIDTH 300)
(define HEIGHT 300)

(define PSEUDO-RANDOM 295)

; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; N is one of:
; – 0
; – (add1 N)
; Interpretation:
;     The natural numbers including 0.

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; N -> [List-of Posn]
; generates n random Posns in [0,WIDTH) by [0,HEIGHT)
(define (random-posns n)
  (build-list
    n
    (lambda (i)
      (make-posn (random WIDTH) (random HEIGHT)))))

(check-satisfied (random-posns 3)
                 (n-inside-playground? 3))

; N -> [List-of Posn]
; generates n (make-posn PSEUDO-RANDOM PSEUDO-RANDOM)
(define (random-posns/bad n)
  (build-list
   n
   (lambda (i)
     (make-posn PSEUDO-RANDOM PSEUDO-RANDOM))))

(check-satisfied (random-posns/bad 3)
                 (n-inside-playground? 3))

; NOTE -------------------------------------------------------------------------

; This specification does not test whether the values in the Posns
; are truly random.

; END NOTE ---------------------------------------------------------------------

; N -> [[List-of Posn] -> Boolean]
; Does l have the length n and are
; 0 <= x < WIDTH and 0 <= y < HEIGHT?
(define (n-inside-playground? n)
  (lambda (l)
    (local (; Posn -> Boolean
            ; Is the x-value of p 0 <= x < WIDTH
            ; and the y-value of p 0 <= y < HEIGHT?
            (define (within-bounds? p)
              (and (<= 0 (posn-x p))
                   (< (posn-x p) WIDTH)
                   (<= 0 (posn-y p))
                   (< (posn-y p) HEIGHT))))
      (and (= (length l) n)
           (andmap within-bounds? l)))))
         

(check-expect ((n-inside-playground? 5) (list (make-posn 229 228)
                                              (make-posn 131 168)
                                              (make-posn 33 214)))
              #false)
(check-expect ((n-inside-playground? 3) (list (make-posn 229 228)
                                              (make-posn 131 168)
                                              (make-posn 33 214)
                                              (make-posn 212 246)
                                              (make-posn 3 27)))
              #false)
(check-expect ((n-inside-playground? 5) (list (make-posn 229 228)
                                              (make-posn 131 4000)
                                              (make-posn 33 214)))
              #false)
(check-expect ((n-inside-playground? 5) (list (make-posn 229 228)
                                              (make-posn 131 168)
                                              (make-posn 5000 214)))
              #false)

(check-expect ((n-inside-playground? 3) (list (make-posn 229 228)
                                              (make-posn 131 168)
                                              (make-posn 33 214)))
              #true)