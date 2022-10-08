;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex333) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Dir.v2 -> N
; Count the number of files in dir.
(define (how-many dir)
  (local (; LOFD -> N
          ; Count the number of files in lofd.
          (define (lofd-how-many lofd)
            (cond
              [(empty? lofd) 0]
              [else
               (local ((define FIRST (first lofd)))
                 ; – IN –
                 (cond
                   [(string? FIRST)
                    (+ 1 (lofd-how-many (rest lofd)))]
                   [(dir? FIRST)
                    (+ (how-many FIRST)
                       (lofd-how-many (rest lofd)))]))])))
    ; – IN –
    (lofd-how-many (dir-content dir))))

(check-expect (how-many (make-dir "Empty" '()))
              0)
(check-expect (how-many CODE)
              2)
(check-expect (how-many DOCS)
              1)
(check-expect (how-many LIBS)
              3)
(check-expect (how-many TEXT)
              3)
(check-expect (how-many TS)
              7)

; Dir.v2 -> N
; Count the number of files in dir.
(define (how-many.v2 dir)
  (local (; LOFD -> N
          ; Count the number of files in lofd.
          (define (lofd-how-many.v2 lofd)
            (foldl (lambda (c acc)
                     (if (string? c)
                         (+ 1 acc)
                         (+ (how-many c) acc)))
                   0
                   lofd)))
    ; – IN –
    (lofd-how-many.v2 (dir-content dir))))

(check-expect (how-many.v2 (make-dir "Empty" '()))
              0)
(check-expect (how-many.v2 CODE)
              2)
(check-expect (how-many.v2 DOCS)
              1)
(check-expect (how-many.v2 LIBS)
              3)
(check-expect (how-many.v2 TEXT)
              3)
(check-expect (how-many.v2 TS)
              7)
