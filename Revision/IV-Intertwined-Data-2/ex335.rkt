;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex335) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define-struct dir [name dirs files])
; A Dir.v3 is a structure: 
;   (make-dir String Dir* File*)
; Interpretation:
;    The string represents the name of the directory,
;    Dir* its subdirectories and File* the files it contains.

(define-struct file [name size content])
; A File.v3 is a structure:
;   (make-file String N String)
; Interpretation:
;    The first string represents the name of the file,
;    the natural number its size, and the string its content.

; A Dir* is one of: 
; – '()
; – (cons Dir.v3 Dir*)
; Interpretation:
;    A list of directories.
 
; A File* is one of: 
; – '()
; – (cons File.v3 File*)
; Interpretation:
;    A list of files.

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define HANG (make-file "hang" 8 "hang file"))
(define DRAW (make-file "draw" 2 "draw file"))
(define CODE (make-dir "Code" '() (list HANG DRAW)))

(define READ1 (make-file "read!" 19 "read! file"))
(define DOCS (make-dir "Docs" '() (list READ1)))

(define LIBS (make-dir "Libs" (list CODE DOCS) '()))

(define PART1 (make-file "part1" 99 "part1 file"))
(define PART2 (make-file "part2" 52 "part2 file"))
(define PART3 (make-file "part3" 17 "part3 file"))
(define TEXT (make-dir "Text" '() (list PART1 PART2 PART3)))

(define READ2 (make-file "read!" 10 "read! file"))
(define TS (make-dir "TS" (list TEXT LIBS) (list READ2)))
