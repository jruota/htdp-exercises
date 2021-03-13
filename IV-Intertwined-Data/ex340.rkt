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

; Dir -> [List-of String]
; List the names of all files and directories in dir.
(define (ls dir)
  (sort
   (append
    (map (lambda (x) (dir-name x)) (dir-dirs dir))
    (map (lambda (x) (file-name x)) (dir-files dir)))
   string<?))

(check-expect (ls (make-dir "empty" '() '()))
              '())
(check-expect (ls TS)
              (sort (list "Libs" "read!" "Text") string<?))