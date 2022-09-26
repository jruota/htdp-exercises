;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex314) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define-struct no-parent [])
(define NP (make-no-parent))
(define-struct child [father mother name date eyes])

; An FT (short for family tree) is one of: 
; – NP
; – (make-child FT FT String N String)

; An FF (short for family forest) is a [List-of FT].
; Interpretation:
;     A family forest represents several
;     families (say, a town) and their ancestor trees.

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define CARL (make-child NP NP "Carl" 1926 "green"))
(define BETTINA (make-child NP NP "Bettina" 1926 "green"))

(define FRED (make-child NP NP "Fred" 1966 "pink"))

(define ADAM (make-child CARL BETTINA "Adam" 1950 "hazel"))
(define DAVE (make-child CARL BETTINA "Dave" 1955 "black"))
(define EVA (make-child CARL BETTINA "Eva" 1965 "blue"))

(define GUSTAV (make-child FRED EVA "Gustav" 1988 "brown"))

(define FF1 (list CARL BETTINA))
(define FF2 (list FRED EVA))
(define FF3 (list FRED EVA CARL))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; FF -> Boolean
; Does the forest contain any child with "blue" eyes? 
(define (blue-eyed-child-in-forest? a-forest)
  (ormap blue-eyed-child? a-forest))

(check-expect (blue-eyed-child-in-forest? FF1) #false)
(check-expect (blue-eyed-child-in-forest? FF2) #true)
(check-expect (blue-eyed-child-in-forest? FF3) #true)

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
