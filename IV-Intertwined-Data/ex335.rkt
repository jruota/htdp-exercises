;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex335) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define-struct file [name size content])
; A File.v3 is a structure: 
;   (make-file String N String)

(define-struct dir.v3 [name dirs files])
; A Dir.v3 is a structure: 
;   (make-dir.v3 String Dir* File*)
 
; A Dir* is one of: 
; – '()
; – (cons Dir.v3 Dir*)
 
; A File* is one of: 
; – '()
; – (cons File.v3 File*)

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define CODE
  (make-dir.v3 "Code"
               '()
               (list (make-file "hang" 8 "")
                     (make-file "draw" 2 ""))))
(define DOCS
  (make-dir.v3 "Docs"
               '()
               (list (make-file "read!" 19 ""))))
(define LIBS
  (make-dir.v3 "Libs"
               (list CODE DOCS)
               '()))
(define TEXT
  (make-dir.v3 "Text"
               '()
               (list (make-file "part1" 99 "")
                     (make-file "part2" 52 "")
                     (make-file "part3" 17 ""))))
(define TS
  (make-dir.v3 "TS"
               (list TEXT LIBS)
               (list (make-file "read!" 10 ""))))