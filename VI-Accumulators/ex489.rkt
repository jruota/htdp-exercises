;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex489) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [List-of Number] -> [List-of Number]
; converts a list of relative to absolute distances
; the first number represents the distance to the origin
(define (relative->absolute l)
  (cond
    [(empty? l) '()]
    [else (local ((define rest-of-l
                    (relative->absolute (rest l)))
                  (define adjusted
                    (add-to-each (first l) rest-of-l)))
            ; – IN –
            (cons (first l) adjusted))]))

(check-expect (relative->absolute '()) '())
(check-expect (relative->absolute '(50 40 70 30 30))
              '(50 90 160 190 220))

; Number [List-of Number] -> [List-of Number]
; adds n to each number on l
(define (add-to-each n l)
;  (cond
;    [(empty? l) '()]
;    [else
;     (cons (+ n (first l))
;           (add-to-each n (rest l)))]))
  (map (lambda (x) (+ x n)) l))

(check-expect (add-to-each 10 '()) '())
(check-expect (add-to-each 0 (list 1 2 3 4 5))
              (list 1 2 3 4 5))
(check-expect (add-to-each 10 (list 1 2 3 4 5))
              (list 11 12 13 14 15))
(check-expect (cons 50 (add-to-each 50 '(40 110 140 170)))
              '(50 90 160 190 220))
 