;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex423) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; An N is one of:
; – 0
; – (add1 N)
; Interpretation:
;     The natural numbers.

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define INPUT-ERROR "integer greater than 0 expected")

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; String N -> [List-of String]
; Produce a list of string chunks of size n.
(define (partition s n)
  (local ((define letters (explode s))

          ; [List-of 1String] -> [List-of String]
          ; Do the actual work.
          (define (main lo1s)
            (cond
              [(empty? lo1s) '()]
              [else
               (cons (implode (take lo1s n))
                     (main (drop lo1s n)))])))
    ; – IN –
    (cond
      [(and (integer? n) (> n 0)) (main letters)]
      [else (error INPUT-ERROR)])))

(check-error (partition "hello world" 0)
             INPUT-ERROR)

(check-expect (partition "" 2)
              '())
(check-expect (partition "hello world" 3)
              (list "hel" "lo " "wor" "ld"))

; [List-of X] N -> [List-of [List-of X]]
; Return the first n items from lox
; or as many as possible if there are less than n.
(define (take lox n)
  (cond
    [(empty? lox) '()]
    [(zero? n) '()]
    [else (cons (first lox) (take (rest lox) (sub1 n)))]))

(check-expect (take '() 4)
              '())
(check-expect (take (explode "abcdefghij") 1)
              (list "a"))
(check-expect (take (explode "abcdefghij") 3)
              (list "a" "b" "c"))
(check-expect (take (explode "abcdefghij") 20)
              (list "a" "b" "c" "d" "e" "f" "g" "h" "i" "j"))

; [List-of X] N -> [List-of X]
; Remove the first n items from lox
; or everything if there are less than n items.
(define (drop lox n)
  (cond
    [(empty? lox) '()]
    [(zero? n) lox]
    [else
     (drop (rest lox) (sub1 n))]))

(check-expect (drop '() 4)
              '())
(check-expect (drop (explode "abcdefghij") 1)
              (explode "bcdefghij"))
(check-expect (drop (explode "abcdefghij") 3)
              (explode "defghij"))
(check-expect (drop (explode "abcdefghij") 20)
              '())
