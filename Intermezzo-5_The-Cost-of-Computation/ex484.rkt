;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex484) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Nelon -> Number
; determines the smallest 
; number on l
(define (infL l)
  (cond
    [(empty? (rest l)) (first l)]
    [else (local ((define s (infL (rest l))))
            (if (< (first l) s) (first l) s))]))

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; (infL (list 3 2 1 0))
; ==
; s_0 = (infL (list 2 1 0))
; (if (< 3 s_0) 3 s_0)

; (infL (list 2 1 0))
; ==
; s_1 = (infL (list 1 0))
; (if (< 2 s_1) 2 s_1)

; (infL (list 1 0))
; ==
; s_2 = (infL (list 0))
; (if (< 1 s_2) 1 s_2)

; (infL (list 0))
; ==
; 0                         -> s_2 = 0

; (if (< 1 0) 1 0)
; ==
; 0                         -> s_1 = 0

; (if (< 2 0) 2 0)
; ==
; 0                         -> s_0 = 0

; (if (< 3 0) 3 0)
; ==
; 0                         -> (infL (list 3 2 1 0)) = 0

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; In the best case, the smallest number is the first item on a list. In that
; case only one function call is needed to determine the result.
; In the worst case, the smallest number is at the end of the list. Then as many
; function calls as the list is long are necessary to determine the result.
