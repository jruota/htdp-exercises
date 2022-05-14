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

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define HANG (make-file "hang" 8 "hang text"))
(define DRAW (make-file "draw" 2 "draw text"))
(define CODE (make-dir.v3 "Code" '() (list HANG DRAW)))

(define READ1 (make-file "read!" 19 "read this text"))
(define DOCS (make-dir.v3 "Docs" '() (list READ1)))

(define LIBS (make-dir.v3 "Libs" (list CODE DOCS) '()))

(define PART1 (make-file "part1" 99 "part1 text"))
(define PART2 (make-file "part2" 52 "part2 text"))
(define PART3 (make-file "part3" 17 "part3 text"))
(define TEXT (make-dir.v3 "Text" '() (list PART1 PART2 PART3)))

(define READ2 (make-file "read!" 10 "read this text"))
(define TS (make-dir.v3 "TS" (list TEXT LIBS) (list READ2)))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Dir.v3 -> N
; Return how many files the directory dir contains.
(define (how-many dir)
  (+ (length (dir.v3-files dir))
     (foldl (lambda (x y) (+ (how-many x) y))
            0
            (dir.v3-dirs dir))))

(check-expect (how-many (make-dir.v3 "Empty" '() '())) 0)
(check-expect (how-many TS) 7)
(check-expect (how-many TEXT) 3)
(check-expect (how-many LIBS) 3)
(check-expect (how-many CODE) 2)
(check-expect (how-many DOCS) 1)
