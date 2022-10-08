;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex336) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Dir.v3 -> N
; Count the number of files in dir.
(define (how-many dir)
  (local ((define FILES (dir-files dir))
          (define DIRS (dir-dirs dir)))
    ; – IN –
    (+ (length FILES)
       (foldl (lambda (d acc) (+ acc (how-many d)))
              0
              DIRS))))

(check-expect (how-many (make-dir "Empty" '() '()))
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

; Dir.v3 -> N
; Count the number of files in dir.
(define (how-many.v2 dir)
  (local ((define FILES (dir-files dir))
          (define DIRS (dir-dirs dir))

          ; File* -> N
          ; Count the number of files in f*.
          (define (files-how-many f*)
            (length f*))

          ; Dir* -> N
          ; Count the number of files contained
          ; in all directories in d*.
          (define (dirs-how-many d*)
            (cond
              [(empty? d*) 0]
              [else
               (+ (how-many.v2 (first d*))
                  (dirs-how-many (rest d*)))])))
    ; – IN –
    (+ (files-how-many FILES)
       (dirs-how-many DIRS))))

(check-expect (how-many.v2 (make-dir "Empty" '() '()))
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
