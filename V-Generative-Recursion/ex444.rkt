;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex444) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; An N is one of:
; – 0
; – (add1 N)
; Interpretation:
;     The natural numbers.

; An NorFalse is one of:
; – N
; – #false
; Interpretation:
;     A natural number or #false.

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; N[>= 1] N[>= 1] -> N
; Find the greatest common divisor of n and m.
(define (gcd-structural S L)
  (largest-common (divisors S S) (divisors S L)))

(check-expect (gcd-structural 1 1) 1)
(check-expect (gcd-structural 24 18) 6)
 
; N[>= 1] N[>= 1] -> [List-of N]
; Compute the divisors of l smaller or equal to k.
(define (divisors k l)
  (cond
    [(< k 1) '()]
    [else
     (if (= (remainder l k) 0)
         (cons k (divisors (sub1 k) l))
         (divisors (sub1 k) l))]))

(check-expect (divisors 1 24) (list 1))
(check-expect (divisors 24 24) (list 24 12 8 6 4 3 2 1))
(check-expect (divisors 24 23) (list 23 1))

; NOTE ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; The following function assumes the following:
; – the arguments given are not empty lists,
; – there is at least one common number.

; END NOTE ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; [NEList-of N] [NEList-of N] -> NorFalse
; Find the largest number common to both k and l.
; If there is no such number, return #false.
(define (largest-common k l)
  (local (; [NEList-of N] [NEList-of N] -> NorFalse
          ; Find the first number common to both k and l.
          ; If there is no such number, return #false.
          (define (ne-largest-common k0 l0)
            (if (member? (first k0) l0)
                (first k0)
                (largest-common (rest k) l))))
    ; – IN –
    (if (empty? k)
        #false
        (ne-largest-common (sort k >) l))))

(check-expect (largest-common '() '()) #false)
(check-expect (largest-common '() (list 1 2 3 4 5)) #false)
(check-expect (largest-common (list 1 2 3 4 5) '()) #false)
(check-expect (largest-common (list 1 2 3 4 5) (list 6 7 8 9 10 11 12 13 14 15))
              #false)
(check-expect (largest-common (list 1) (list 1)) 1)
(check-expect (largest-common (list 908 723 374 349 617 179)
                              (list 377 81 676 415 723 454))
              723)

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Why do you think divisors consumes two numbers?
; Why does it consume S as the first argument in both uses?

; If one is looking for the greatest common divisor (gcd) of two numbers, it
; is sufficient to find all divisors that are less than or equal to the smaller
; of the two, as any number greater than the smaller cannot be a divisor of said
; number. This reduces the amount of numbers that need to be checked.

; Which number is consumed first is a design choice, but it needs to be applied
; consistently for the function to work in its current design. Here, the first
; number simply is the upper limit for the possible divisors.

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;(time (gcd-structural 101135853 45014640))