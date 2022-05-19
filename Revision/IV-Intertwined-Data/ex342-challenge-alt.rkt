;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex342-challenge-alt) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require htdp/dir)

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
  (find-in-dir dir f))

(check-expect (find TS "no-such-file") '())
(check-expect (find TS "read!") (list (list "TS" "read!")
                                      (list "TS" "Libs" "Docs" "read!")))
(check-expect (find TS "part1") (list (list "TS" "Text" "part1")))
(check-expect (find TS "hang") (list (list "TS" "Libs" "Code" "hang")))

; Dir String -> [List-of [List-of Path]]
; Return the path to file f if it is in dir
; or one of its subdirectories.
(define (find-in-dir dir f)
  (map (lambda (x) (cons (dir-name dir) x))
       (append (find-file (dir-files dir) f)
               (find-in-dirs (dir-dirs dir) f))))

(check-expect (find-in-dir TS "no-such-file") '())
(check-expect (find-in-dir TS "read!") (list (list "TS" "read!")
                                             (list "TS" "Libs" "Docs" "read!")))
(check-expect (find-in-dir TS "part1") (list (list "TS" "Text" "part1")))
(check-expect (find-in-dir TS "hang") (list (list "TS" "Libs" "Code" "hang")))

; [List-of Dir] String -> [List-of [List-of Path]]
; If file f is in any of the directories in lod,
; return its Path(s).
(define (find-in-dirs lod f)
  (cond
    [(empty? lod) '()]
    [else
     (append
      (map (lambda (x) (cons (dir-name (first lod)) x))
           (append (find-file (dir-files (first lod)) f)
                   (find-in-dirs (dir-dirs (first lod)) f)))
      (find-in-dirs (rest lod) f))]))

(check-expect (find-in-dirs (dir-dirs TS) "no-such-file") '())
(check-expect (find-in-dirs (dir-dirs TS) "read!")
              (list (list "Libs" "Docs" "read!")))
(check-expect (find-in-dirs (dir-dirs TS) "part3") (list (list "Text" "part3")))
(check-expect (find-in-dirs (dir-dirs TS) "draw")
              (list (list "Libs" "Code" "draw")))

; [List-of File] String -> [List-of [List-of Path]]
; If file f is in lof, return its name.
(define (find-file lof f)
  (cond
    [(empty? lof) '()]
    [else
     (if (string=? (file-name (first lof)) f)
         (cons (list f) (find-file (rest lof) f))
         (find-file (rest lof) f))]))

(check-expect (find-file (dir-files TS) "no-such-file") '())
(check-expect (find-file (dir-files TS) "read!") (list (list "read!")))
