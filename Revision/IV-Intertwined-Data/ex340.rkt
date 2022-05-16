;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex340) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require htdp/dir)

; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; (define-struct file [name size content])
; A File is a structure: 
;   (make-file String N String)

; (define-struct dir [name dirs files])
; A Dir is a structure: 
;   (make-dir String [List-of Dir] [List-of File])

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define TS (create-dir "TS"))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Dir String -> Boolean
; List the names of all files and directories in dir.
(define (ls dir)
  (local ((define directories (dir-dirs dir))
          (define files (dir-files dir)))
    ; – IN –
;    (append (map (lambda (x) (dir-name x)) directories)
;            (map (lambda (x) (file-name x)) files))))
    (append (map (lambda (x) (file-name x)) files)
            (map (lambda (x) (dir-name x)) directories))))

(check-member-of (ls TS)
                 (list "Libs" "Text" "read!")
                 (list "Libs" "read!" "Text")
                 (list "read!" "Libs" "Text")
                 (list "read!" "Text" "Libs")
                 (list "Text" "Libs" "read!")
                 (list "Text" "read!" "Libs"))
