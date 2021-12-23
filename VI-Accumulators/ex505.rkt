;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex505) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define INPUT-ERROR "is-prime? expects a positive integer greater or equal 1")

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; N -> Boolean
; Is n a prime number?
(define (is-prime? n)
  (local (; N [>=1] -> Boolean
          ; Is n divisible by the
          ; numbers in the range [2 x]?
          (define (main x)
            (cond
              [(= x 1) #true]
              [else
               (and (not (= (modulo n x) 0))
                    (main (sub1 x)))])))
    ; – IN –
    (cond
      [(and (integer? n) (>= n 0))
       (if (or (= n 1) (= n 0))
           #false
           (main (sub1 n)))]
      [else
       (error INPUT-ERROR)])))

(check-expect (is-prime? 0) #false)
(check-expect (is-prime? 1) #false)
(check-expect (is-prime? 2) #true)
(check-expect (is-prime? 75) #false)
(check-expect (is-prime? 113) #true)
(check-expect (is-prime? 10007) #true)

(check-error (is-prime? 3.14) INPUT-ERROR)
(check-error (is-prime? -1) INPUT-ERROR)

; N -> Boolean
; Is n a prime number?
(define (is-prime?.v2 n)
  (local (; N [>=1] -> Boolean
          ; Is n divisible by the
          ; numbers in the range [2 x]?
          ; The accumulator accu is a intermediate
          ; result telling whether n is divisible
          ; by the numbers in [(add1 x) (sub1 n)].
          (define (is-prime?/a x accu)
            (cond
              [accu #false]
              [else
               (if (= x 1)
                   #true
                   (is-prime?/a (sub1 x)
                                (= (modulo n x) 0)))])))
    ; – IN –
    (cond
      [(and (integer? n) (>= n 0))
       (if (or (= n 1) (= n 0))
           #false
           (is-prime?/a (sub1 n) #false))]
      [else
       (error INPUT-ERROR)])))

(check-expect (is-prime?.v2 0) #false)
(check-expect (is-prime?.v2 1) #false)
(check-expect (is-prime?.v2 2) #true)
(check-expect (is-prime?.v2 75) #false)
(check-expect (is-prime?.v2 113) #true)
(check-expect (is-prime?.v2 10007) #true)

(check-error (is-prime?.v2 3.14) INPUT-ERROR)
(check-error (is-prime?.v2 -1) INPUT-ERROR)

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define size 10)
(define list-of-999331 (build-list size (lambda (x) 999331)))

(time (foldr (lambda (x y) (and (is-prime? x) y)) #true list-of-999331))
(time (foldr (lambda (x y) (and (is-prime?.v2 x) y)) #true list-of-999331))
