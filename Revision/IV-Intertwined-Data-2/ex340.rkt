;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex340) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require htdp/dir)

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define TS (create-dir "TS"))
(define TS-2 (create-dir "TS-2"))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Dir -> [List-of String]
; List all files and directories in dir
; in alphabetical order.
(define (ls dir)
  (sort
   (append (map (lambda (f) (file-name f))
                (dir-files dir))
           (map (lambda (d) (dir-name d))
                (dir-dirs dir)))
   (lambda (s1 s2) (string<? (string-downcase s1) (string-downcase s2)))))

(check-expect (ls (make-dir "Empty" '() '()))
              '())
(check-expect (ls TS)
              (list "Libs" "read!" "Text"))
(check-expect (ls TS-2)
              (list "Libs" "Text"))
