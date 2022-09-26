;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex312) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

; FT -> [List-of String]
; Return a list of all the eye colors
; in ft (includes duplicates).
(define (eye-colors ft)
  (cond
    [(no-parent? ft) '()]
    [else
     (cons (child-eyes ft)
           (append (eye-colors (child-father ft))
                   (eye-colors (child-mother ft))))]))

(check-expect (eye-colors NP) '())
(check-expect (eye-colors BETTINA) (list "green"))
(check-expect (eye-colors EVA) (list "blue" "green" "green"))
(check-expect (eye-colors GUSTAV)
              (list "brown" "pink" "blue" "green" "green"))
