;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex330) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A Dir.v1 (short for directory) is one of: 
; – '()
; – (cons File.v1 Dir.v1)
; – (cons Dir.v1 Dir.v1)
 
; A File.v1 is a String. 

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Translate the directory tree in figure 127 into a data representation
; according to model 1.

(define DIRTREE1 (list
                  "read!"
                  (list "part1" "part2" "part3")
                  (list
                   (list "hang" "draw")
                   (list "read!"))))

(define DIRTREE2 (cons
                  "read!"
                  (cons
                   (cons "part1" (cons "part2" (cons "part3" '())))
                   (cons
                    (cons
                     (cons "hang" (cons "draw" '()))
                     (cons
                      (cons "read!" '())
                      '()))
                    '()))))

(define CODE (cons "hang" (cons "draw" '())))
(define DOCS (cons "read!" '()))
(define TEXT (cons "part1" (cons "part2" (cons "part3" '()))))
(define LIBS (cons CODE (cons DOCS '())))
(define TS (cons "read!" (cons TEXT (cons LIBS '()))))

(check-expect DIRTREE1 DIRTREE2)
(check-expect DIRTREE1 TS)