;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex338) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require htdp/dir)

; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; (define-struct file [name size content])
; A File is a structure: 
;   (make-file String N String)

; (define-struct dir [name dirs files])
; A Dir is a structure: 
;   (make-dir String Dir* File*)
 
; A Dir* is one of: 
; – '()
; – (cons Dir Dir*)
 
; A File* is one of: 
; – '()
; – (cons File File*)

; An N is one of:
; – 0
; – (add1 N)
; Interpretation:
;     The counting numbers,
;     including 0.

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define CODE
  (make-dir "Code"
               '()
               (list (make-file "hang" 8 "")
                     (make-file "draw" 2 ""))))
(define DOCS
  (make-dir "Docs"
               '()
               (list (make-file "read!" 19 ""))))
(define LIBS
  (make-dir "Libs"
               (list CODE DOCS)
               '()))
(define TEXT
  (make-dir "Text"
               '()
               (list (make-file "part1" 99 "")
                     (make-file "part2" 52 "")
                     (make-file "part3" 17 ""))))
(define TS
  (make-dir "TS"
               (list TEXT LIBS)
               (list (make-file "read!" 10 ""))))

; my current Desktop, easy to check for correctness;
; run "(how-many DESKTOP)" in the Interactions Area
; and compare with the Desktop
(define DESKTOP (create-dir "/home/jruota/Desktop"))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; Tests have been omitted and can be found in ex336.rkt.

; Dir -> N
; Determine how many files dir contains.
(define (how-many dir)
  (+ (how-many-files (dir-files dir))
     (how-many-in-dir* (dir-dirs dir))))

; Files* -> N
; Determine how many files files* contains,
; i.e. its length.
(define (how-many-files files*)
  (length files*))

; Dir* -> N
; Determine how many files the directories
; in dir* contain.
(define (how-many-in-dir* dir*)
  (cond
    [(empty? dir*) 0]
    [else
     (+ (how-many (first dir*))
        (how-many-in-dir* (rest dir*)))]))
