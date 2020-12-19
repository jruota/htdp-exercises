;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex260) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A Nelon is one of:
;     – (cons Number '())
;     – (cons Number Nelon)
; Interpretation:
;     A non-empty list of numbers.

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; for testing

(define ONE
  (list 25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1))

(define TWO
  (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Nelon -> Number
; determines the smallest 
; number on l
(define (inf l)
  (cond
    [(empty? (rest l))
     (first l)]
    [else
     (if (< (first l)
            (inf (rest l)))
         (first l)
         (inf (rest l)))]))

(check-expect (inf '(656))
              656)
(check-expect (inf '(203 375 656 364 59 467))
              59)

; Nelon -> Number
; determines the smallest 
; number on l
(define (inf-1 l)
  (cond
    [(empty? (rest l))
     (first l)]
    [else
     (local
       ((define inf-of-rest (inf-1 (rest l))))
       ; – IN –
       (if (< (first l)
              inf-of-rest)
           (first l)
           inf-of-rest))]))

(check-expect (inf-1 '(656))
              656)
(check-expect (inf-1 '(203 375 656 364 59 467))
              59)

; Nelon -> Number
; determines the largest 
; number on l
(define (sup l)
  (cond
    [(empty? (rest l))
     (first l)]
    [else
     (if (> (first l)
            (sup (rest l)))
         (first l)
         (sup (rest l)))]))

(check-expect (sup '(59))
              59)
(check-expect (sup '(203 375 656 364 59 467))
              656)

; Nelon -> Number
; determines the largest 
; number on l
(define (sup-1 l)
  (cond
    [(empty? (rest l))
     (first l)]
    [else
     (local
       ((define sup-in-rest (sup (rest l))))
       ; – IN –
       (if (> (first l)
              sup-in-rest)
           (first l)
           sup-in-rest))]))

(check-expect (sup-1 '(59))
              59)
(check-expect (sup-1 '(203 375 656 364 59 467))
              656)

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; The explanation why "inf-1" and "sup-1" are faster than "inf" and "sup" in
; some of the cases, is given in the paragraph preceding the exercise:
;
; Here the local expression shows up in the middle of a cond expression. It
; defines a constant whose value is the result of a natural recursion. Now
; recall that the evaluation of a local expression evaluates the definitions
; once before proceeding to the body, meaning (inf (rest l)) is evaluated once
; while the body of the local expression refers to the result twice. Thus, local
; saves the re-evaluation of (inf (rest l)) at each stage in the computation.