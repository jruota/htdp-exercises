;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex160) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A Son.L is one of: 
; – empty 
; – (cons Number Son.L)

; A Son.R is one of: 
; – empty 
; – (cons Number Son.R)
; 
; Constraint If s is a Son.R, 
; no number occurs twice in s

; Son is used when it 
; applies to Son.L and Son.R

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Son
(define es '())

(define s1.L (cons 1 (cons 1 '())))
(define s1.R (cons 1 '()))

(define set123-version1 (cons 1 (cons 2 (cons 3 '()))))
(define set123-version2 (cons 1 (cons 3 (cons 2 '()))))

(define set23-version1 (cons 2 (cons 3 '())))
(define set23-version2 (cons 3 (cons 2 '())))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Number Son -> Boolean
; is x in s
(define (in? x s)
  (member? x s))

(check-expect (in? 1 '())
              #false)
(check-expect (in? 1 (cons 2 (cons 10 (cons 3 '()))))
              #false)
(check-expect (in? 1 (cons 2 (cons 10 (cons 3 (cons 1 '())))))
              #true)

; Number Son.L -> Son.L
; removes x from s
(define (set-.L x s)
  (remove-all x s))

(check-expect (set-.L 1 s1.L)
              es)
(check-satisfied (set-.L 1 set123-version1)
                 not-member-1?)
(check-satisfied (set-.L 1 set123-version2)
                 not-member-1?)
		
; Number Son.R -> Son.R
; removes x from s
(define (set-.R x s)
  (remove x s))
 
(check-expect (set-.R 1 s1.R)
              es)
(check-satisfied (set-.R 1 set123-version1)
                 not-member-1?)
(check-satisfied (set-.R 1 set123-version2)
                 not-member-1?)

; Son -> Boolean
; #true if 1 is not a member of s; #false otherwise
(define (not-member-1? s)
  (not (in? 1 s)))

(check-expect (not-member-1? '())
              #true)
(check-expect (not-member-1? (cons 2 (cons 10 (cons 3 '()))))
              #true)
(check-expect (not-member-1? (cons 2 (cons 10 (cons 3 (cons 1 '())))))
              #false)

; Son.L -> Son.L
; Add value x to s.
(define (set+.L x s)
  (cons x s))

(check-satisfied (set+.L 1 '())
                 member-1?)
(check-satisfied (set+.L 1 (cons 1 (cons 2 (cons 3 '()))))
                 member-1?)
(check-satisfied (set+.L 1 (cons 2 (cons 3 (cons 4 '()))))
                 member-1?)

; Son.R -> Son.R
; Add value x to s
; if x is not in s.
(define (set+.R x s)
  (if (in? x s)
      s
      (cons x s)))

(check-satisfied (set+.R 1 '())
                 member-1?)
(check-satisfied (set+.R 1 (cons 1 (cons 2 (cons 3 '()))))
                 member-1?)
(check-satisfied (set+.R 1 (cons 2 (cons 3 (cons 4 '()))))
                 member-1?)
(check-expect (set+.R 1 (cons 1 (cons 2 (cons 3 '()))))
              (cons 1 (cons 2 (cons 3 '()))))

; Son -> Boolean
; Return #true if 1 is a member of s,
; #false otherwise.
(define (member-1? s)
  (in? 1 s))

(check-expect (member-1? '())
              #false)
(check-expect (member-1? (cons 2 (cons 3 (cons 4 '()))))
              #false)
(check-expect (member-1? (cons 2 (cons 3 (cons 4 (cons 1 '())))))
              #true)