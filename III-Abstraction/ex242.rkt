;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex242) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A [Maybe X] is one of: 
; – #false 
; – X

; A [List-of ITEM] is one of: 
; – '() 
; – (cons ITEM [List-of ITEM])

; A [Maybe String] is one of:
; – #false
; – String
#false
"hello"

; A [Maybe [List-of String]] is one:
; – #false
; – [List-of String]
#false
'()
(cons "hello" '())
(cons "hello" (cons "world" '()))

; A [List-of [Maybe String]] is one of:
; – '()
; – (cons [Maybe String] [List-of [Maybe String]])
'()
(cons #false '())
(cons "hello" (cons #false (cons "world" '())))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; String [List-of String] -> [Maybe [List-of String]]
; returns the remainder of los starting with s 
; #false otherwise
(define (occurs s los)
  (cond
    [(empty? los) #false]
    [(cons? los)
     (if (string=? s (first los))
         (rest los)
         (occurs s (rest los)))]))

(check-expect (occurs "a" (list "b" "a" "d" "e"))
              (list "d" "e"))
(check-expect (occurs "a" (list "b" "c" "d"))
              #f)
