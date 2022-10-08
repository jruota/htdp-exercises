;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex343) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require htdp/dir)

; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A Path is [List-of String].
; Interpretation:
;     Directions into a directory tree.

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define TS (create-dir "TS"))
(define EMPTY (create-dir "EMPTY"))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Dir -> [List-of Path]
; List the paths to all files and directories
; contained in the directory d.
(define (ls-R d)
  (local ((define DIR-NAME (dir-name d))
          (define FILES-AND-DIRS
            (sort
             (append
              (map (lambda (d) (dir-name d)) (dir-dirs d))
              (map (lambda (f) (file-name f)) (dir-files d)))
             (lambda (x y)
               (string<? (string-downcase x) (string-downcase y)))))

          ; [List-of Dir] -> [List-of Path]
          ; List the paths to all files and directories
          ; contained in the list of directories lod.
          (define (ls-lod-R lod)
            (cond
              [(empty? lod) '()]
              [else
               (append (ls-R (first lod))
                       (ls-lod-R (rest lod)))])))
    ; – IN –
    (append
     (map (lambda (x) (cons DIR-NAME (cons x '()))) FILES-AND-DIRS)
     (map (lambda (x) (cons DIR-NAME x)) (ls-lod-R (dir-dirs d))))))

(check-expect (ls-R EMPTY)
              '())
(check-expect (ls-R TS)
              (list (list "TS" "Libs")
                    (list "TS" "read!")
                    (list "TS" "Text")
                    (list "TS" "Libs" "Code")
                    (list "TS" "Libs" "Docs")
                    (list "TS" "Libs" "Code" "draw")
                    (list "TS" "Libs" "Code" "hang")
                    (list "TS" "Libs" "Docs" "read!")
                    (list "TS" "Text" "part1")
                    (list "TS" "Text" "part2")
                    (list "TS" "Text" "part3")))
