;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex331) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A Dir.v1 (short for directory) is one of: 
; – '()
; – (cons File.v1 Dir.v1)
; – (cons Dir.v1 Dir.v1)
 
; A File.v1 is a String.

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define HANG "hang")
(define DRAW "draw")
(define CODE (list HANG DRAW))

(define READ1 "read!")
(define DOCS (list READ1))

(define LIBS (list CODE DOCS))

(define PART1 "part1")
(define PART2 "part2")
(define PART3 "part3")
(define TEXT (list PART1 PART2 PART3))

(define READ2 "read!")
(define TS (list READ2 TEXT LIBS))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Dir.v1 -> N
; Count the number of files in dir.
(define (how-many dir)
  (cond
    [(empty? dir) 0]
    [else
     (local ((define FIRST (first dir))
             (define REST (rest dir)))
       ; – IN –
       (cond
         [(string? FIRST)
          (+ 1 (how-many REST))]
         [(cons? FIRST)
          (+ (how-many FIRST)
             (how-many REST))]))]))

(check-expect (how-many '())
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

; Dir.v1 -> N
; Count the number of files in dir.
(define (how-many.v2 dir)
  (foldl (lambda (item acc)
           (if (string? item)
               (+ 1 acc)
               (+ (how-many.v2 item) acc)))
         0
         dir))

(check-expect (how-many.v2 '())
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
