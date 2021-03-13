;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex333) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)

; DATA DEFINIITONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define-struct dir [name content])
; A Dir.v2 is a structure: 
;   (make-dir String LOFD)
 
; An LOFD (short for list of files and directories) is one of:
; – '()
; – (cons File.v2 LOFD)
; – (cons Dir.v2 LOFD)
 
; A File.v2 is a String.

; An N is one of:
; – 0
; – (add1 N)
; Interpretation:
;     The natural numbers,
;     including 0.

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define CODE
  (make-dir "Code" (list "hang" "draw")))
(define DOCS
  (make-dir "Docs" (list "read!")))
(define LIBS
  (make-dir "Libs" (list CODE DOCS)))
(define TEXT
  (make-dir "Text" (list "part1" "part2" "part3")))
(define TS
  (make-dir "TS" (list "read!" TEXT LIBS)))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Dir.v2 -> N
; Determine how many files dir contains.
(define (how-many dir)
  (how-many-in-lofd (dir-content dir)))

(check-expect (how-many (make-dir "empty" '()))
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

; LOFD -> N
; Determine how many files lofd contains.
(define (how-many-in-lofd lofd)
  (cond
    [(empty? lofd) 0]
    [(string? (first lofd))
     (+ 1 (how-many-in-lofd (rest lofd)))]
    [else
     (+ (how-many (first lofd))
        (how-many-in-lofd (rest lofd)))]))

(check-expect (how-many-in-lofd '())
              0)
(check-expect (how-many-in-lofd (dir-content CODE))
              2)
(check-expect (how-many-in-lofd (dir-content DOCS))
              1)
(check-expect (how-many-in-lofd (dir-content LIBS))
              3)
(check-expect (how-many-in-lofd (dir-content TEXT))
              3)
(check-expect (how-many-in-lofd (dir-content TS))
              7)

; Dir.v2 -> N
; Determine how many files dir contains.
(define (how-many.v2 dir)
  (how-many-in-lofd.v2 (dir-content dir)))

(check-expect (how-many.v2 (make-dir "empty" '()))
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

; LOFD -> N
; Determine how many files lofd contains.
(define (how-many-in-lofd.v2 lofd)
  (foldl (lambda (x init)
           (+ (if (string? x)
                  1
                  (how-many.v2 x))
              init))
         0 lofd))

(check-expect (how-many-in-lofd.v2 '())
              0)
(check-expect (how-many-in-lofd.v2 (dir-content CODE))
              2)
(check-expect (how-many-in-lofd.v2 (dir-content DOCS))
              1)
(check-expect (how-many-in-lofd.v2 (dir-content LIBS))
              3)
(check-expect (how-many-in-lofd.v2 (dir-content TEXT))
              3)
(check-expect (how-many-in-lofd.v2 (dir-content TS))
              7)