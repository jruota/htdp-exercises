;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex313) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define-struct no-parent [])
(define-struct child [father mother name date eyes])
(define NP (make-no-parent))
; An FT is one of: 
; – NP
; – (make-child FT FT String N String)

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Oldest Generation:
(define Carl (make-child NP NP "Carl" 1926 "green"))
(define Bettina (make-child NP NP "Bettina" 1926 "green"))
 
; Middle Generation:
(define Adam (make-child Carl Bettina "Adam" 1950 "hazel"))
(define Dave (make-child Carl Bettina "Dave" 1955 "black"))
(define Eva (make-child Carl Bettina "Eva" 1965 "blue"))
(define Fred (make-child NP NP "Fred" 1966 "pink"))
 
; Youngest Generation: 
(define Gustav (make-child Fred Eva "Gustav" 1988 "brown"))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; The initial function only returns #false, no matter the input, because only
; the base case ever returns a value, and that value is always #false.

; FT -> Boolean
; Return #true only when a proper ancestor,
; not the given child itself, has blue eyes.
(define (blue-eyed-ancestor? an-ftree)
  (cond
    [(no-parent? an-ftree) #false]
    [else
     (local ((define father (child-father an-ftree))
             (define mother (child-mother an-ftree)))
       ; – IN –
       (or (blue-eyed? father)
           (blue-eyed? mother)
           (blue-eyed-ancestor? father)
           (blue-eyed-ancestor? mother)))]))

(check-expect (blue-eyed-ancestor? Carl) #false)
(check-expect (blue-eyed-ancestor? Bettina) #false)
(check-expect (blue-eyed-ancestor? Adam) #false)
(check-expect (blue-eyed-ancestor? Dave) #false)
(check-expect (blue-eyed-ancestor? Eva) #false)
(check-expect (blue-eyed-ancestor? Fred) #false)
(check-expect (blue-eyed-ancestor? Gustav) #true)

; FT -> Boolean
; Does the top child (top node) have blue eyes?
(define (blue-eyed? ft)
  (cond
    [(no-parent? ft) #false]
    [else
     (string=? (child-eyes ft) "blue")]))

(check-expect (blue-eyed? Gustav) #false)
(check-expect (blue-eyed? Eva) #true)
