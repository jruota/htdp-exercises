;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex313) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define-struct no-parent [])
(define NP (make-no-parent))
(define-struct child [father mother name date eyes])

; An FT (short for family tree) is one of: 
; – NP
; – (make-child FT FT String N String)

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define CARL (make-child NP NP "Carl" 1926 "green"))
(define BETTINA (make-child NP NP "Bettina" 1926 "green"))

(define FRED (make-child NP NP "Fred" 1966 "pink"))

(define ADAM (make-child CARL BETTINA "Adam" 1950 "hazel"))
(define DAVE (make-child CARL BETTINA "Dave" 1955 "black"))
(define EVA (make-child CARL BETTINA "Eva" 1965 "blue"))

(define GUSTAV (make-child FRED EVA "Gustav" 1988 "brown"))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Explain why this function fails one of its tests.
; What is the result of (blue-eyed-ancestor? A) no matter which A you choose?
;     Independent of its input, this function always returns #false (its base
;     case). There is no case in which it returns #true. It always completely
;     traverses the complete family tree until it reaches this base case. 

; FT -> Boolean
; Does an-ftree contain an ancestor
; structure with "blue" in the eyes field? 
(define (blue-eyed-ancestor? an-ftree)
  (cond
    [(no-parent? an-ftree) #false]
    [else
     (or (blue-eyed-ancestor? (child-father an-ftree))
         (blue-eyed-ancestor? (child-mother an-ftree)))]))

(check-expect (blue-eyed-ancestor? EVA) #false)
(check-expect (blue-eyed-ancestor? GUSTAV) #true)

; FT -> Boolean
; Does an-ftree contain an ancestor
; structure with "blue" in the eyes field? 
(define (blue-eyed-ancestor?.v2 an-ftree)
  (cond
    [(no-parent? an-ftree) #false]
    [else
     (or (blue-eyed-child? (child-father an-ftree))
         (blue-eyed-child? (child-mother an-ftree)))]))

(check-expect (blue-eyed-ancestor?.v2 EVA) #false)
(check-expect (blue-eyed-ancestor?.v2 GUSTAV) #true)

; from the book ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; FT -> Boolean
; does an-ftree contain a child
; structure with "blue" in the eyes field 
(define (blue-eyed-child? an-ftree)
  (cond
    [(no-parent? an-ftree) #false]
    [else (or (string=? (child-eyes an-ftree) "blue")
              (blue-eyed-child? (child-father an-ftree))
              (blue-eyed-child? (child-mother an-ftree)))]))
 
(check-expect (blue-eyed-child? CARL) #false)
(check-expect (blue-eyed-child? GUSTAV) #true)
