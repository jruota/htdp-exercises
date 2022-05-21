;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex344) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require htdp/dir)
(require racket/list)

; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A Path is [List-of String].
; interpretation directions into a directory tree

; (define-struct file [name size content])
; A File is a structure: 
;   (make-file String N String)

; (define-struct dir [name dirs files])
; A Dir is a structure: 
;   (make-dir String [List-of Dir] [List-of File])

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define TS (create-dir "TS"))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Dir String -> [List-of [List-of Path]]
; Produce a path to file f in dir.
; If no such file exists, return #false.
(define (find dir f)
  (local (; [List-of File] -> [List-of [List-of Path]]
          ; If file f is in lof, return its name.
          (define (find-file lof)
            (cond
              [(empty? lof) '()]
              [else
               (if (string=? (file-name (first lof)) f)
                   (cons (list f) (find-file (rest lof)))
                   (find-file (rest lof)))]))

          ; [List-of Dir] -> [List-of [List-of Path]]
          ; If file f is in any of the directories in lod,
          ; return its Path(s).
          (define (find-in-dirs lod)
            (cond
              [(empty? lod) '()]
              [else
               (append
                (map (lambda (x) (cons (dir-name (first lod)) x))
                     (append (find-file (dir-files (first lod)))
                             (find-in-dirs (dir-dirs (first lod)))))
                (find-in-dirs (rest lod)))]))

          ; Dir -> [List-of [List-of Path]]
          ; Return the path to file f if it is in dir0
          ; or one of its subdirectories.
          (define (find-in-dir dir0)
            (map (lambda (x) (cons (dir-name dir0) x))
                 (append (find-file (dir-files dir0))
                         (find-in-dirs (dir-dirs dir0))))))
    ; – IN –
    (find-in-dir dir)))

(check-expect (find TS "no-such-file") '())
(check-expect (find TS "read!") (list (list "TS" "read!")
                                      (list "TS" "Libs" "Docs" "read!")))
(check-expect (find TS "part1") (list (list "TS" "Text" "part1")))
(check-expect (find TS "hang") (list (list "TS" "Libs" "Code" "hang")))

; from ex343.rkt ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; adapted to list directories as well

; Dir -> [List-of [List-of Path]]
; List the paths to all files and directories contained in dir.
(define (ls-R dir)
  (local (; [List-of File] -> [List-of [List-of Path]]
          ; Return the paths to the files in lof,
          ; i.e. in the current directory.
          (define (file-names lof)
            (map (lambda (x) (list (file-name x))) lof))

          ; [List-of Dir] -> [List-of [List-of Path]]
          ; Return the paths to all files in all directories
          ; in lod.
          (define (find-in-dirs lod)
            (foldl
             (lambda (x y) (append (find-in-dir x) y))
             '()
             lod))

          ; Dir -> [List-of [List-of Path]]
          ; List the paths to all files in dir.
          (define (find-in-dir dir)
            (local ((define NAME (dir-name dir)))
              ; – IN –
              (cons (list NAME)
              (map
               (lambda (x) (cons NAME x))
               (append
                (file-names (dir-files dir))
                (find-in-dirs (dir-dirs dir))))))))
    ; – IN –
    (find-in-dir dir)))

(check-expect (ls-R TS)
              (list
               (list "TS")
               (list "TS" "read!")
               (list "TS" "Text")
               (list "TS" "Text" "part1")
               (list "TS" "Text" "part2")
               (list "TS" "Text" "part3")
               (list "TS" "Libs")
               (list "TS" "Libs" "Docs")
               (list "TS" "Libs" "Docs" "read!")
               (list "TS" "Libs" "Code")
               (list "TS" "Libs" "Code" "draw")
               (list "TS" "Libs" "Code" "hang")))

; Made my computer crash.
;(check-expect (member? (ls-R TS)
;                       (permutations
;                        (list
;                         (list "TS")
;                         (list "TS" "read!")
;                         (list "TS" "Text")
;                         (list "TS" "Text" "part1")
;                         (list "TS" "Text" "part2")
;                         (list "TS" "Text" "part3")
;                         (list "TS" "Libs")
;                         (list "TS" "Libs" "Code")
;                         (list "TS" "Libs" "Code" "hang")
;                         (list "TS" "Libs" "Code" "draw")
;                         (list "TS" "Libs" "Docs")
;                         (list "TS" "Libs" "Docs" "read!"))))
;              #true)