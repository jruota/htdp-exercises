;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex504) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A Digit is a Number in the range [0, 9].

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; [List-of Digit] -> Number
; Produce the corresponding number from lod.
; The first item on lod is the most significant digit.
(define (to10 lod)
  (cond
    [(empty? lod) 0]
    [else
     (+ (* (first lod) (expt 10 (sub1 (length lod))))
        (to10 (rest lod)))]))

(check-expect (to10 '()) 0)
(check-expect (to10 '(1 0 2)) 102)

; [List-of Digit] -> Number
; Produce the corresponding number from lod.
; The first item on lod is the most significant digit.
(define (to10.v2 lod)
  (local (; [List-of Digit] Number -> Number
          ; Produce the corresponding number from lod0.
          ; The accumulator accu collects the result of
          ; the first n digits from lod0.
          (define (to10/a lod0 accu)
            (cond
              [(empty? lod0) accu]
              [else
               (to10/a (rest lod0)
                       (+ (* (first lod0) (expt 10 (sub1 (length lod0))))
                          accu))])))
    ; – IN –
    (to10/a lod 0)))

(check-expect (to10.v2 '()) 0)
(check-expect (to10.v2 '(1 0 2)) 102)

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define size 1000)
(define random-list (build-list size (lambda (x) (random 10))))
(time (to10 random-list))
(time (to10.v2 random-list))
