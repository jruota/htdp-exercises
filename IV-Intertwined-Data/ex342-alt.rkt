;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex342-alt) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

; A PathOrFalse is one of:
; – #false
; – Path
; Interpretation:
;     The path to a file if it exists or #false.

; A [X]OrFalse is one of:
; – #false
; – [X]
; Interpretation:
;     An X or #false.

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

; Dir String -> PathOrFalse
; If there is a file or directory fod
; somewhere in directory dir,
; return the path to it, #false otherwise.
(define (find dir fod)
  (local (; [List-of File] -> StringOrFalse
          ; Return the name of file f
          ; if it is in lof,
          ; #false otherwise.
          (define (lof-find lof)
            (cond
              [(empty? lof) #false]
              [else
               (if (string=? (file-name (first lof)) fod)
                   (file-find (first lof))
                   (lof-find (rest lof)))]))

          ; [List-of Dir] -> PathOrFalse
          ; If file f exists in one of the
          ; directories in lod, return the
          ; path to the file, #false elsewhere.
          (define (lod-find lod)
            (cond
              [(empty? lod) #false]
              [else
               (cond
                 [(string=? (dir-name (first lod)) fod)
                  (list (dir-name (first lod)))]
                 [(find? (first lod) fod)
                  (dir-find (first lod))]
                 [else
                  (lod-find (rest lod))])]))

          ; File -> String
          ; Return the file name of file f.
          (define (file-find f)
            (file-name f))

          ; Dir -> PathOrFalse
          ; If file f exists is somewhere in directory dir,
          ; return the path to file f, #false otherwise.
          (define (dir-find d)
            (local ((define IN-SUBDIRS (lod-find (dir-dirs d))))
              ; – IN –
              (cond
                [(not (false? (lof-find (dir-files d))))
                 (list (dir-name d)
                       fod)]
                [(not (false? IN-SUBDIRS))
                 (cons (dir-name d)
                       IN-SUBDIRS)]
                [else #false]))))
    ; – IN –
    (dir-find dir)))

(check-expect (find TS "part4")
              #false)
(check-expect (find TS "part3")
              (list "TS" "Text" "part3"))
(check-expect (find TS "draw")
              (list "TS" "Libs" "Code" "draw"))
(check-expect (find TS "read!")
              (list "TS" "read!"))
(check-expect (find (first (dir-dirs TS)) "read!")
              (list "Libs" "Docs" "read!"))
(check-expect (find TS "Images")
              #false)
(check-expect (find TS "Text")
              (list "TS" "Text"))
(check-expect (find TS "Libs")
              (list "TS" "Libs"))
(check-expect (find TS "Code")
              (list "TS" "Libs" "Code"))
(check-expect (find TS "Docs")
              (list "TS" "Libs" "Docs"))
(check-expect (find TS-2 "Text")
              (list "TS-2" "Libs" "Text"))

; Dir String -> [List-of Path]
; Produce the list of all paths
; that lead to file or directory fod in dir.
; If there is no file or directory fod, return
; an empty list.
(define (find-all dir0 fod)
  (local (; String [List-of [List-of String]] -> [List-of [List-of String]]
          ; Prepend all elements of los with s,
          ; i.e. prepend all Paths with the
          ; parent directory.
          (define (prepend s los)
            (map (lambda (p) (cons s p)) los))

          ; [List-of File] -> Boolean
          ; Is file f among the files
          ; in lof?
          (define (lof-find? lof)
            (ormap (lambda (x) (string=? (file-name x) fod)) lof))

          ; [List-of Dir] -> [List-of [List-of Path]]
          ; If there is one or more files f
          ; in the directory trees in lod,
          ; return the paths to the file(s).
          (define (lod-find lod)
            (cond
              [(empty? lod) '()]
              [else
               (cond
                 [(string=? (dir-name (first lod)) fod)
                  (cons
                   (list (dir-name (first lod)))
                   (append
                    (find-all (first lod) fod)
                    (lod-find (rest lod))))]
                 [else
                  ; append reduces lists of empty lists to one empty list
                  (append (dir-find (first lod))
                          (lod-find (rest lod)))])]))

          ; Dir -> [List-of [List-of Path]]
          ; If there is one or more files f
          ; in the directory tree dir1, return
          ; the paths to the file(s).
          (define (dir-find dir1)
            (cond
              [(lof-find? (dir-files dir1))
               (cons (list (dir-name dir1) fod)
                     (prepend (dir-name dir1)
                              (lod-find (dir-dirs dir1))))]
              [else
               (prepend (dir-name dir1)
                        (lod-find (dir-dirs dir1)))])))
    ; – IN –
    (dir-find dir0)))

(check-expect (find-all TS-2 "part4")
              '())
(check-expect (find-all TS-2 "part3")
              (list (list "TS-2" "Libs" "Text" "part3")
                    (list "TS-2" "Text" "part3")
                    (list "TS-2" "Text" "Text" "part3")))
(check-expect (find-all (second (dir-dirs TS)) "part4")
              '())
(check-expect (find-all TS "part4")
              '())
(check-expect (find-all TS "part3")
              (list (list "TS" "Text" "part3")))
(check-expect (find-all TS "draw")
              (list (list "TS" "Libs" "Code" "draw")))
(check-expect (find-all TS "read!")
              (list (list "TS" "read!")
                    (list "TS" "Libs" "Docs" "read!")))
(check-expect (find-all TS "Images")
              '())
(check-expect (find-all TS "Text")
              (list (list "TS" "Text")))
(check-expect (find-all TS "Libs")
              (list (list "TS" "Libs")))
(check-expect (find-all TS "Code")
              (list (list "TS" "Libs" "Code")))
(check-expect (find-all TS "Docs")
              (list (list "TS" "Libs" "Docs")))
(check-expect (find-all TS-2 "Text")
              (list (list "TS-2" "Libs" "Text")
                    (list "TS-2" "Text")
                    (list "TS-2" "Text" "Text")))

; from ex399-alt.rkt +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

; Dir String -> Boolean
; Does the file or directory fod
; occur in the directory tree dir?
(define (find? dir fod)
  (or (ormap (lambda (x) (string=? (file-name x) fod)) (dir-files dir))
      (ormap (lambda (x) (string=? (dir-name x) fod)) (dir-dirs dir))
      (ormap (lambda (x) (find? x fod)) (dir-dirs dir))))

(check-expect (find? TS "read!")
              #true)
(check-expect (find? TS "part4")
              #false)
(check-expect (find? TS "hang")
              #true)
(check-expect (find? TS "Images")
              #false)
(check-expect (find? TS "Code")
              #true)
