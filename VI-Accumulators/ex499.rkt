;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex499) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [List-of Number] -> Number
; Compute the product of all numbers
; in lon.
(define (product lon)
  (local (; [NE-List-of Number] -> Number
          ; Compute the product of all numbers
          ; in the non-empty lon0.
          (define (ne-product lon0)
            (cond
              [(empty? (rest lon0)) (first lon0)]
              [else
               (* (first lon0)
                  (ne-product (rest lon0)))])))
    ; – IN –
    (cond
      [(empty? lon) 0]
      [else
       (ne-product lon)])))

(check-expect (product '()) 0)
(check-expect (product (list 3 3 3)) (* 3 3 3))
(check-expect (product (list 1 8 4 6 5)) (* 1 8 4 6 5))
(check-expect (product (list 1 8 4 0 6 5)) 0)

; [List-of Number] -> Number
; Compute the product of all numbers
; in lon.
(define (product.v2 lon)
  (local (; [NE-List-of Number] N -> Number
          ; Compute the product of all numbers
          ; in the non-empty lon0.
          ; The accumulator accu represents the result
          ; of the product of the first (- n 1) numbers
          ; in lon0.
          (define (ne-product/a lon0 accu)
            (cond
              [(empty? (rest lon0))
               (* (first lon0) accu)]
              [else
               (ne-product/a (rest lon0)
                             (* (first lon0) accu))])))
    ; – IN –
    (cond
      [(empty? lon) 0]
      [else
       (ne-product/a lon 1)])))

(check-expect (product.v2 '()) 0)
(check-expect (product.v2 (list 3 3 3)) (* 3 3 3))
(check-expect (product.v2 (list 1 8 4 6 5)) (* 1 8 4 6 5))
(check-expect (product.v2 (list 1 8 4 0 6 5)) 0)

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define size 5000)
(define list-of-random-numbers
  (build-list size (lambda (x) (+ (random (+ size 1)) 1))))

(time (product list-of-random-numbers))
(time (product.v2 list-of-random-numbers))
