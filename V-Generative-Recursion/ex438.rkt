;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex438) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; N[>= 1] N[>= 1] -> N
; finds the greatest common divisor of n and m
(define (gcd-structural n m)
  (local (; N -> N
          ; determines the gcd of n and m less than i
          (define (greatest-divisor-<= i)
            (cond
              [(= i 1) 1]
              [else
               (if (= (remainder n i) (remainder m i) 0)
                   i
                   (greatest-divisor-<= (- i 1)))])))
    ; – IN –
    (greatest-divisor-<= (min n m))))

(check-expect (gcd 6 25) 1)
(check-expect (gcd 18 24) 6)

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; In your words: how does greatest-divisor-<= work?

; The data for this function are two natural numbers, so any structural approach
; increases or decreases a natural number by one for each recursive call.

; The trivial case is when the gcd is equal to 1, since there are no more valid
; divisors less than 1. Therefore 1 is the final solution.

; --- (Why does the locally defined greatest-divisor-<= recur on (min n m)?) ---

; Since the function is to return the greatest common divisor, the obvious first
; guess is to choose the smaller of the two numbers given, which is also the
; greatest possible solution. The greater of the two numbers cannot be a valid
; solution because the lesser number is not divisible by it. Should it not be
; the right choice, delegate the problem to a recursive call with the guess
; decreased by one.

; The guess is the solution to the problem if both input numbers have a
; remainder of 0 if divided by the guess.

; The function terminates when the guess reaches the value 1 (the signature
; demands the inputs to be natural numbers greater than or equal than 1 and
; since the first guess is one of those input numbers the function is
; guaranteed to terminate).