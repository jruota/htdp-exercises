;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex332) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define-struct dir [name content])
; A Dir.v2 is a structure: 
;   (make-dir String LOFD)
 
; An LOFD (short for list of files and directories) is one of:
; – '()
; – (cons File.v2 LOFD)
; – (cons Dir.v2 LOFD)
 
; A File.v2 is a String. 

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define HANG "hang")
(define DRAW "draw")
(define CODE (make-dir "Code" (list HANG DRAW)))

(define READ1 "read!")
(define DOCS (make-dir "Docs" (list READ1)))

(define LIBS (make-dir "Libs" (list CODE DOCS)))

(define PART1 "part1")
(define PART2 "part2")
(define PART3 "part3")
(define TEXT (make-dir "Text" (list PART1 PART2 PART3)))

(define READ2 "read!")
(define TS (make-dir "TS" (list READ2 TEXT LIBS)))

TS
