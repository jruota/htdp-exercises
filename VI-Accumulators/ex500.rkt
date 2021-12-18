;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex500) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [List-of X] -> N
; Return the number of items in lst.
(define (how-many lst)
  (cond
    [(empty? lst) 0]
    [else
     (+ 1 (how-many (rest lst)))]))

(check-expect (how-many '()) 0)
(check-expect (how-many (list 1 'a "hello" (make-posn 3 9))) 4)

; [List-of X] -> N
; Return the number of items in lst.
(define (how-many.v2 lst)
  (local (; [List-of X] N -> N
          ; Return the number of items in lst.
          ; The accumulator accu collects the number
          ; of the first (- n 1) items in lst0.
          (define (how-many/a lst0 accu)
            (cond
              [(empty? lst0) accu]
              [else
               (how-many/a (rest lst0) (+ accu 1))])))
    ; – IN –
    (how-many/a lst 0)))

(check-expect (how-many.v2 '()) 0)
(check-expect (how-many.v2 (list 1 'a "hello" (make-posn 3 9))) 4)

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; The performance of how-many is O(n) where n is the length of the list.
; Does the accumulator version improve on this?

    ; No, the accumulator version still needs to iterate over the whole list.

; When you evaluate (how-many some-non-empty-list) by hand, n applications of
; add1 are pending by the time the function reaches '() — where n is the number
; of items on the list. Computer scientists sometime say that how-many needs
; O(n) space to represent these pending function applications.
; Does the accumulator reduce the amount of space needed to compute the result?

    ; Yes, as the accumulator is the intermidiary result and thus all
    ; applications of + or add 1 are resolved by the time the functions reaches
    ; the end of the list.

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define size 50000)
(define lst (build-list size (lambda (x) 0)))

(time (how-many lst))
(time (how-many.v2 lst))
