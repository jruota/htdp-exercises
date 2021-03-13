;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex331) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A Dir.v1 (short for directory) is one of: 
; – '()
; – (cons File.v1 Dir.v1)
; – (cons Dir.v1 Dir.v1)
 
; A File.v1 is a String. 

; An N is one of:
; – 0
; – (add1 N)
; Interpretation:
;     The natural numbers,
;     including 0.

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define CODE (cons "hang" (cons "draw" '())))
(define DOCS (cons "read!" '()))
(define TEXT (cons "part1" (cons "part2" (cons "part3" '()))))
(define LIBS (cons CODE (cons DOCS '())))
(define TS (cons "read!" (cons TEXT (cons LIBS '()))))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Dir.v1 -> N
; Determine how many files dir contains.
(define (how-many dir)
  (cond
    [(empty? dir) 0]
    [(string? (first dir))
     (+ 1 (how-many (rest dir)))]
    [else
     (+ (how-many (first dir))
        (how-many (rest dir)))]))

(check-expect (how-many '())
              0)
(check-expect (how-many LIBS)
              3)
(check-expect (how-many DOCS)
              1)
(check-expect (how-many CODE)
              2)
(check-expect (how-many TEXT)
              3)
(check-expect (how-many TS)
              7)
