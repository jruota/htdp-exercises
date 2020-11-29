;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex250) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Number -> [List-of Number]
; tabulates sin between n 
; and 0 (incl.) in a list
(define (tab-sin n)
  (cond
    [(= n 0) (list (sin 0))]
    [else
     (cons
      (sin n)
      (tab-sin (sub1 n)))]))

(check-within (tab-sin 0)
              (list (sin 0))
              .0001)
(check-within (tab-sin 4)
              (list (sin 4) (sin 3) (sin 2) (sin 1) (sin 0))
              .0001)

; Number -> [List-of Number]
; tabulates sin between n 
; and 0 (incl.) in a list
(define (tab-sin.v2 n)
  (tabulate n sin))

(check-within (tab-sin.v2 0)
              (list (sin 0))
              .0001)
(check-within (tab-sin.v2 4)
              (list (sin 4) (sin 3) (sin 2) (sin 1) (sin 0))
              .0001)

; Number -> [List-of Number]
; tabulates sqrt between n 
; and 0 (incl.) in a list
(define (tab-sqrt n)
  (cond
    [(= n 0) (list (sqrt 0))]
    [else
     (cons
      (sqrt n)
      (tab-sqrt (sub1 n)))]))

(check-within (tab-sqrt 0)
              (list (sqrt 0))
              .0001)
(check-within (tab-sqrt 4)
              (list (sqrt 4) (sqrt 3) (sqrt 2) (sqrt 1) (sqrt 0))
              .0001)

; Number -> [List-of Number]
; tabulates sqrt between n 
; and 0 (incl.) in a list
(define (tab-sqrt.v2 n)
  (tabulate n sqrt))

(check-within (tab-sqrt.v2 0)
              (list (sqrt 0))
              .0001)
(check-within (tab-sqrt.v2 4)
              (list (sqrt 4) (sqrt 3) (sqrt 2) (sqrt 1) (sqrt 0))
              .0001)

; Number [Number -> Number] -> [List-of Number]
; Tabulate f between n
; and 0 (inclusive) in a list.
(define (tabulate n f)
  (cond
    [(= n 0)
     (list (f 0))]
    [else
     (cons
      (f n)
      (tabulate (sub1 n) f))]))

(check-within (tabulate 0 sin)
              (list (sin 0))
              .0001)
(check-within (tabulate 4 sqrt)
              (list (sqrt 4) (sqrt 3) (sqrt 2) (sqrt 1) (sqrt 0))
              .0001)