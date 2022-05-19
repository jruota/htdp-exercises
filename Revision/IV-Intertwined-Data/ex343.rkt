;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex343) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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
(define TEXT (create-dir "TS/Text"))
(define LIBS (create-dir "TS/Libs"))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Dir -> [List-of [List-of Path]]
; List the paths to all files contained in dir.
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
              (map
               (lambda (x) (cons NAME x))
               (append
                (file-names (dir-files dir))
                (find-in-dirs (dir-dirs dir)))))))
    ; – IN –
    (find-in-dir dir)))

(check-expect (member? (ls-R TS)
                       (permutations
                        (list
                         (list "TS" "read!")
                         (list "TS" "Text" "part1")
                         (list "TS" "Text" "part2")
                         (list "TS" "Text" "part3")
                         (list "TS" "Libs" "Code" "hang")
                         (list "TS" "Libs" "Code" "draw")
                         (list "TS" "Libs" "Docs" "read!"))))
              #true)

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; [List-of File] -> [List-of [List-of Path]]
; Return the paths to the files in lof,
; i.e. in the current directory.
(define (file-names lof)
  (map (lambda (x) (list (file-name x))) lof))

(check-expect (file-names (dir-files TS)) (list (list "read!")))

; Dir -> [List-of [List-of Path]]
; List the paths to all files in dir.
(define (find-in-dir dir)
  (local ((define NAME (dir-name dir)))
    ; – IN –
    (map
     (lambda (x) (cons NAME x))
     (append
      (file-names (dir-files dir))
      (find-in-dirs (dir-dirs dir))))))

; [List-of Dir] -> [List-of [List-of Path]]
; Return the paths to all files in all directories
; in lod.
(define (find-in-dirs lod)
  (foldl
   (lambda (x y) (append (find-in-dir x) y))
   '()
   lod))

; --------

(check-expect (find-in-dir TEXT)
              (list (list "Text" "part1")
                    (list "Text" "part2")
                    (list "Text" "part3")))
(check-expect (member? (find-in-dir LIBS)
                       (permutations
                        (list
                         (list "Libs" "Code" "hang")
                         (list "Libs" "Code" "draw")
                         (list "Libs" "Docs" "read!"))))
              #true)

; --------

(check-expect (find-in-dirs '()) '())
(check-expect (member? (find-in-dirs (dir-dirs TS))
                       (permutations
                        (list
                         (list "Text" "part1")
                         (list "Text" "part2")
                         (list "Text" "part3")
                         (list "Libs" "Code" "hang")
                         (list "Libs" "Code" "draw")
                         (list "Libs" "Docs" "read!"))))
              #true)
