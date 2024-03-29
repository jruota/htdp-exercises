;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex337) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define-struct file [name size content])
; A File.v3 is a structure: 
;   (make-file String N String)

(define-struct dir.v3 [name dirs files])
; A Dir.v3 is a structure: 
;   (make-dir.v3 String [List-of Dir.v3] [List-of File.v3])

; An N is one of:
; – 0
; – (add1 N)
; Interpretation:
;     The counting numbers,
;     including 0.

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

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Dir.v3 -> N
; Determine how many files dir contains.
(define (how-many dir)
  (+ (how-many-files (dir.v3-files dir))
     (how-many-in-dir* (dir.v3-dirs dir))))

(check-expect (how-many (make-dir.v3 "Empty" '() '()))
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

; Files* -> N
; Determine how many files files* contains,
; i.e. its length.
(define (how-many-files files*)
  (length files*))

(check-expect (how-many-files '())
              0)
(check-expect (how-many-files (list (make-file "part1" 99 "")
                                    (make-file "part2" 52 "")
                                    (make-file "part3" 17 "")))
              3)

; Dir* -> N
; Determine how many files the directories
; in dir* contain.
(define (how-many-in-dir* dir*)
  (foldr (lambda (x init)
           (+ (how-many x)
              init))
         0
         dir*))

(check-expect (how-many-in-dir* '())
              0)
(check-expect (how-many-in-dir* (list DOCS))
              1)
(check-expect (how-many-in-dir* (list CODE))
              2)
(check-expect (how-many-in-dir* (list TEXT))
              3)
(check-expect (how-many-in-dir* (list TEXT DOCS CODE))
              6)