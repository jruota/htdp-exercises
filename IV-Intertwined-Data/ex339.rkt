;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex339) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require htdp/dir)

; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; (define-struct file [name size content])
; A File is a structure: 
;   (make-file String N String)

; (define-struct dir [name dirs files])
; A Dir is a structure: 
;   (make-dir.v3 String [List-of Dir] [List-of File])

; An N is one of:
; – 0
; – (add1 N)
; Interpretation:
;     The counting numbers,
;     including 0.

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define TS
  (create-dir
   (string-append "/home/jruota/learning/cs/teach-yourself-cs/"
                  "01-HtDP/htdp-exercises/IV-Intertwined-Data/TS")))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Dir String -> Boolean
; Does file occur in the directory tree dir?
(define (find? dir file)
  (local (; [List-of File] -> Boolean
          ; Does file occur in lof of the current working directory?
          (define (file-in-current-dir? lof)
            (ormap (lambda (x) (string=? file (file-name x))) lof))

          ; [List-of Dir] -> Boolean
          ; Does file occur in any of the
          ; directories listed in lod?
          (define (file-in-subdirs? lod)
            (ormap (lambda (x) (find? x file)) lod)))

    ; – IN –

    (or (file-in-current-dir? (dir-files dir))
        (file-in-subdirs? (dir-dirs dir)))))

(check-expect (find? TS "read!")
              #true)
(check-expect (find? TS "part4")
              #false)
(check-expect (find? TS "hang")
              #true)

;; Dir String -> Boolean
;; Does file occur in the directory tree dir?
;(define (find? dir file)
;  (or (file-in-current-dir? (dir-files dir) file)
;      (file-in-subdirs? (dir-dirs dir) file)))
;
;(check-expect (find? TS "read!")
;              #true)
;(check-expect (find? TS "part4")
;              #false)
;(check-expect (find? TS "hang")
;              #true)
;
;; [List-of File] String -> Boolean
;; Does file occur in lof of the current working directory?
;(define (file-in-current-dir? lof file)
;  (ormap (lambda (x) (string=? file (file-name x))) lof))
;
;(check-expect (file-in-current-dir? '() "part1")
;              #false)
;(check-expect (file-in-current-dir?
;               (list
;                (make-file "part1" 0 (make-date 2021 3 13 7 48 40) "")
;                (make-file "part2" 0 (make-date 2021 3 13 7 48 45) "")
;                (make-file "part3" 0 (make-date 2021 3 13 7 48 49) ""))
;               "part4")
;              #false)
;(check-expect (file-in-current-dir?
;               (list
;                (make-file "part1" 0 (make-date 2021 3 13 7 48 40) "")
;                (make-file "part2" 0 (make-date 2021 3 13 7 48 45) "")
;                (make-file "part3" 0 (make-date 2021 3 13 7 48 49) ""))
;               "part2")
;              #true)
;
;; [List-of Dir] String -> Boolean
;; Does file occur in any of the
;; directories listed in lod?
;(define (file-in-subdirs? lod file)
;  (ormap (lambda (x) (find? x file)) lod))
;
;(check-expect (file-in-subdirs? '() "draw")
;              #false)
;(check-expect (file-in-subdirs? (dir-dirs TS) "draw")
;              #true)
;(check-expect (file-in-subdirs? (dir-dirs (first (dir-dirs TS))) "draw")
;              #true)
;(check-expect (file-in-subdirs? (dir-dirs (first (dir-dirs TS))) "read!")
;              #true)
;(check-expect (file-in-subdirs? (dir-dirs TS) "part4")
;              #false)
;              
