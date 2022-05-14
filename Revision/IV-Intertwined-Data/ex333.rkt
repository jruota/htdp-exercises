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

(define CODE (make-dir "Code" (list "hang" "draw")))
(define DOCS (make-dir "Docs" (list "read!")))
(define LIBS (make-dir "Libs" (list CODE DOCS)))
(define TEXT (make-dir "Text" (list "part1" "part2" "part3")))
(define TS (make-dir "TS" (list TEXT "read!" LIBS)))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Dir.v2 -> N
; Return how many files the directory dir contains.
(define (how-many dir)
  (local (; LOFD -> N
          ; Return the number of files in
          ; directory dir.
          (define (how-many-lofd lofd)
            (cond
              [(empty? lofd) 0]
              [else
               (local ((define item (first lofd)))
                 ; – IN –
                 (cond
                   [(string? item)
                    (+ 1 (how-many-lofd (rest lofd)))]
                   [(dir? item)
                    (+ (how-many item)
                       (how-many-lofd (rest lofd)))]))])))
    ; – IN –
    (how-many-lofd (dir-content dir))))

(check-expect (how-many (make-dir "Empty" '())) 0)
(check-expect (how-many TS) 7)
(check-expect (how-many TEXT) 3)
(check-expect (how-many LIBS) 3)
(check-expect (how-many CODE) 2)
(check-expect (how-many DOCS) 1)
