;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex426) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [List-of Number] -> [List-of Number]
; produces a sorted version of alon
(define (quick-sort< alon)
  (cond
    [(empty? alon) '()]
    [(empty? (rest alon)) alon]
    [else
     (append (quick-sort< (smallers alon (first alon)))
             (list (first alon))
             (quick-sort< (largers alon (first alon))))]))

(check-expect (quick-sort< '()) '())
(check-expect (quick-sort< (list 23)) (list 23))
(check-expect (quick-sort< (list 11 8 14 7)) (list 7 8 11 14))
(check-expect (quick-sort< (list 11 9 2 18 12 14 4 1))
              (list 1 2 4 9 11 12 14 18))

; [List-of Number] Number -> [List-of Number]
; Return all numbers from lon that are less
; than n.
(define (smallers lon n)
  (cond
    [(empty? lon) '()]
    [else
     (if (< (first lon) n)
         (cons (first lon) (smallers (rest lon) n))
         (smallers (rest lon) n))]))

(check-expect (smallers (list 11 8 14 7) 11)
              (list 8 7))
(check-expect (smallers (list 11 9 2 18 12 14 4 1) 11)
              (list 9 2 4 1))

; [List-of Number] Number -> [List-of Number]
; Return all numbers from lon that are greater
; than n.
(define (largers lon n)
  (cond
    [(empty? lon) '()]
    [else
     (if (> (first lon) n)
         (cons (first lon) (largers (rest lon) n))
         (largers (rest lon) n))]))

(check-expect (largers (list 11 8 14 7) 11)
              (list 14))
(check-expect (largers (list 11 9 2 18 12 14 4 1) 11)
              (list 18 12 14))

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;(quick-sort< (list 11 8 14 7))
;==
;(append (quick-sort< (list 8 7))
;        (list 11)
;        (quick-sort< (list 14)))
;==
;(append (append (quick-sort< (list 7))
;                (list 8)
;                (quick-sort< '()))
;        (list 11)
;        (quick-sort< (list 14)))
;==
;(append (append (append (quick-sort< '())
;                        (list 7)
;                        (quick-sort< '()))
;                (list 8)
;                (quick-sort< '()))
;        (list 11)
;        (quick-sort< (list 14)))
;==
;(append (append (append '()
;                         (list 7)
;                        '())
;                (list 8)
;                '())
;        (list 11)
;        (quick-sort< (list 14)))
;==
;(append (append (list 7)
;                (list 8)
;                '())
;        (list 11)
;        (quick-sort< (list 14)))
;==
;(append (list 7 8)
;        (list 11)
;        (quick-sort< (list 14)))
;==
;(append (list 7 8 11)
;        (append (quick-sort '())
;                (list 14)
;                (quick-sort '())))
;==
;(append (list 7 8 11)
;        (append '()
;                (list 14)
;                '()))
;==
;(append (list 7 8 11)
;        (list 14))
;==
;(list 7 8 11 14)

; OLD VERSION ------------------------------------------------------------------

;(quick-sort< (list 23))
;==
;(append (quick-sort< '())
;        (list 23)
;        (quick-sort< '()))
;==
;(append '()
;        (list 23)
;        '())
;==
;(list 23)

; REVISED VERSION --------------------------------------------------------------

; The revised version saves steps each time there is a list of length one.

;(quick-sort< (list 11 8 14 7))
;==
;(append (quick-sort< (list 8 7))
;        (list 11)
;        (quick-sort< (list 14)))
;==
;(append (append (quick-sort< (list 7))
;                (list 8)
;                (quick-sort< '()))
;        (list 11)
;        (quick-sort< (list 14)))
;==
;(append (append (list 7)
;                (list 8)
;                (quick-sort< '()))
;        (list 11)
;        (quick-sort< (list 14)))
;==
;(append (append (list 7)
;                (list 8)
;                '())
;        (list 11)
;        (quick-sort< (list 14)))
;==
;(append (list 7 8)
;        (list 11)
;        (quick-sort< (list 14)))
;==
;(append (list 7 8 11)
;        (list 14))
;==
;(list 7 8 11 14)

; ------------------------------------------------------------------------------

;(quick-sort< (list 23))
;==
;(list 23)