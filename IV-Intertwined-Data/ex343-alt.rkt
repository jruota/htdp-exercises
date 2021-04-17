;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex343-alt) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require htdp/dir)

; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; (define-struct file [name size content])
; A File is a structure: 
;   (make-file String N String)

; (define-struct dir [name dirs files])
; A Dir is a structure: 
;   (make-dir String [List-of Dir] [List-of File])

; A Path is [List-of String].
; Interpretation:
;     Directions into a directory tree.

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define TS
  (create-dir
   (string-append "/home/jruota/learning/cs/teach-yourself-cs/"
                  "01-HtDP/htdp-exercises/IV-Intertwined-Data/TS")))

(define TS-2
  (create-dir
   (string-append "/home/jruota/learning/cs/teach-yourself-cs/"
                  "01-HtDP/htdp-exercises/IV-Intertwined-Data/TS-2")))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Dir -> [List-of Path]
; List the paths to all files
; and directories contained in dir0.
(define (ls-R dir0)
  (local (; String [List-of [List-of String]] -> [List-of [List-of String]]
          ; Prepend all elements of los with s,
          ; i.e. prepend all Paths with the
          ; parent directory.
          (define (prepend s los)
            (map (lambda (p) (cons s p)) los))

          ; [List-of File] -> [List-of [List-of String]]
          ; Return the names of all files
          ; in lof as a list containing
          ; lists of strings. If there are no
          ; files, return the empty list.
          (define (files-ls lof)
            (map (lambda (f) (list (file-name f))) lof))

          ; [List-of Dir] -> [List-of Path]
          ; Return the paths to all the files
          ; in all the directory trees in lod.
          (define (dirs-ls lod)
            (cond
              [(empty? lod) '()]
              [else
               (append (list (list (dir-name (first lod))))
                       (dir-ls (first lod))
                       (dirs-ls (rest lod)))]))

          ; Dir -> [List-of Path]
          ; Return the paths to all
          ; the files in dir1.
          (define (dir-ls dir1)
            (append (prepend (dir-name dir1)
                             (files-ls (dir-files dir1)))
                    (prepend (dir-name dir1) (dirs-ls (dir-dirs dir1))))))
    ; – IN –
    (dir-ls dir0)))

; NOTE *************************************************************************

; The function "check-expect" is not suitable for testing here, because the
; tests only pass when one lists the elements in the "expected-expression"
; in the right order.
; I used them anyway, because using "check-satisfied" would be too much of a
; hustle.

; END NOTE *********************************************************************
(check-expect (ls-R (make-dir "Empty" empty empty))
              '())
(check-expect (ls-R TS-2)
              (list (list "TS-2" "Libs")
                    (list "TS-2" "Libs" "Text")
                    (list "TS-2" "Libs" "Text" "part1")
                    (list "TS-2" "Libs" "Text" "part2")
                    (list "TS-2" "Libs" "Text" "part3")
                    (list "TS-2" "Text")
                    (list "TS-2" "Text" "part1")
                    (list "TS-2" "Text" "part2")
                    (list "TS-2" "Text" "part3")
                    (list "TS-2" "Text" "Text")
                    (list "TS-2" "Text" "Text" "part1")
                    (list "TS-2" "Text" "Text" "part2")
                    (list "TS-2" "Text" "Text" "part3")))
(check-expect (ls-R TS)
              (list (list "TS" "read!")
                    (list "TS" "Libs")
                    (list "TS" "Libs" "Code")
                    (list "TS" "Libs" "Code" "draw")
                    (list "TS" "Libs" "Code" "hang")
                    (list "TS" "Libs" "Docs")
                    (list "TS" "Libs" "Docs" "read!")
                    (list "TS" "Text")
                    (list "TS" "Text" "part1")
                    (list "TS" "Text" "part2")
                    (list "TS" "Text" "part3")))