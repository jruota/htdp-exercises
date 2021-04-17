;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex344-alt) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

; Dir String -> [List-of Path]
; Produce the list of all paths
; that lead to the file or
; directory fod in dir.
; If there is no file or directory fod,
; return an empty list.
(define (find-all dir0 fod)
  (local (; Path -> Boolean
          ; Is fod the last element in p?
          (define (last? p)
            (local (; NE-Path -> Boolean
                    ; Is fod the last element
                    ; in the non-empty path p?
                    (define (ne-last? p)
                      (cond
                        [(empty? (rest p))
                         (string=? fod (first p))]
                        [else
                         (ne-last? (rest p))])))
              ; – IN –
              (cond
                [(empty? p)
                 #false]
                [else
                 (ne-last? p)])))

          ; Path [List-of Path] -> [List-of Path]
          ; Remove the first occurrence of p
          ; from the list of paths lop.
          (define (remove-path p lop)
            (cond
              [(empty? lop) '()]
              [else
               (if (equal? p (first lop))
                   (rest lop)
                   (cons (first lop)
                         (remove-path p lop)))]))

          ; [List-of Path] -> [List-of Path]
          ; Remove any duplicate paths in lop.
          ; Keep the first occurrence of each path.
          (define (remove-duplicates lop)
            (cond
              [(empty? lop) '()]
              [else
               (if (member? (first lop) (rest lop))
                   (remove-duplicates
                    (cons (first lop)
                          (remove-path (first lop) (rest lop))))
                   (cons (first lop)
                         (remove-duplicates (rest lop))))])))
    ; – IN –
    (remove-duplicates (filter last? (ls-R dir0)))))
               

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

; Dir -> [List-of Path]
; List the paths to all files
; contained in dir0.
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

