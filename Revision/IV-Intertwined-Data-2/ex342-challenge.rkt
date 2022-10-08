;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex342-challenge) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require htdp/dir)

; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A Path is [List-of String].
; Interpretation:
;     Directions into a directory tree.

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define TS (create-dir "TS"))
(define TS-2 (create-dir "TS-2"))
(define TEXT (create-dir "TS/Text"))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Dir String -> [List-of Path]
; Return all paths to the file or directory fod
; in d.
(define (find-all-final d fod)
  (local ((define DIR-NAME (dir-name d))

          ; [List-of Dir] -> [List-of Path]
          ; Return a list of all paths to file or directory fod
          ; in the list of directories lod.
          (define (find-all-dirs lod)
            (cond
              [(empty? lod)
               '()]
              [else
               (local ((define FIRST (first lod))
                       (define FIRST-NAME (dir-name FIRST))
                       (define FIND-ALL (find-all-final FIRST fod))
                       (define FIND-ALL-DIRS (find-all-dirs (rest lod)))
                       (define FOUND-ALL (append FIND-ALL FIND-ALL-DIRS)))
                 ; – IN –
                 (cond
                   [(string=? FIRST-NAME fod)
                    (cons (list FIRST-NAME)
                          FOUND-ALL)]
                   [else
                    FOUND-ALL]))]))

          ; [List-of File] -> [List-of String]
          ; Return a list of all filenames of the files in lof
          ; that match the filename fod.
          (define (find-all-files lof)
            (cond
              [(empty? lof)
               '()]
              [else
               (local ((define FIRST-NAME (file-name (first lof))))
                 ; – IN –
                 (cond
                   [(string=? FIRST-NAME fod)
                    (cons FIRST-NAME
                          (find-all-files (rest lof)))]
                   [else
                    (find-all-files (rest lof))]))])))
    ; – IN –
    (append
     (map (lambda (f) (cons DIR-NAME
                            (cons f '())))
          (find-all-files (dir-files d)))
     (map (lambda (p) (cons DIR-NAME p))
          (find-all-dirs (dir-dirs d))))))

(check-expect (find-all-final TS "no-such-file")
              '())
(check-expect (find-all-final TS "draw")
              (list (list "TS" "Libs" "Code" "draw")))
(check-expect (find-all-final TS "Code")
              (list (list "TS" "Libs" "Code")))
(check-expect (find-all-final TS "read!")
              (list (list "TS" "read!")
                    (list "TS" "Libs" "Docs" "read!")))

(check-expect (find-all-final TS-2 "Text")
              (list (list "TS-2" "Libs" "Text")
                    (list "TS-2" "Text")
                    (list "TS-2" "Text" "Text")))
(check-expect (find-all-final TS-2 "part2")
              (list (list "TS-2" "Libs" "Text" "part2")
                    (list "TS-2" "Text" "part2")
                    (list "TS-2" "Text" "Text" "part2")))

; Dir String -> [List-of Path]
; Return all paths to the file or directory fod
; in d.
(define (find-all d fod)
  (local ((define DIR-NAME (dir-name d)))
    ; – IN –
    (append
     (map (lambda (f) (cons DIR-NAME
                            (cons f '())))
          (find-all-files (dir-files d) fod))
     (map (lambda (p) (cons DIR-NAME p))
          (find-all-dirs (dir-dirs d) fod)))))

(check-expect (find-all TS "no-such-file")
              '())
(check-expect (find-all TS "draw")
              (list (list "TS" "Libs" "Code" "draw")))
(check-expect (find-all TS "Code")
              (list (list "TS" "Libs" "Code")))
(check-expect (find-all TS "read!")
              (list (list "TS" "read!")
                    (list "TS" "Libs" "Docs" "read!")))

(check-expect (find-all TS-2 "Text")
              (list (list "TS-2" "Libs" "Text")
                    (list "TS-2" "Text")
                    (list "TS-2" "Text" "Text")))
(check-expect (find-all TS-2 "part2")
              (list (list "TS-2" "Libs" "Text" "part2")
                    (list "TS-2" "Text" "part2")
                    (list "TS-2" "Text" "Text" "part2")))

; [List-of Dir] String -> [List-of Path]
; Return a list of all paths to file or directory fod
; in the list of directories lod.
(define (find-all-dirs lod fod)
  (cond
    [(empty? lod)
     '()]
    [else
     (local ((define FIRST (first lod))
             (define FIRST-NAME (dir-name FIRST))
             (define FIND-ALL (find-all FIRST fod))
             (define FIND-ALL-DIRS (find-all-dirs (rest lod) fod))
             (define FOUND-ALL (append FIND-ALL FIND-ALL-DIRS)))
       ; – IN –
     (cond
       [(string=? FIRST-NAME fod)
        (cons (list FIRST-NAME)
              FOUND-ALL)]
       [else
        FOUND-ALL]))]))

(check-expect (find-all-dirs '() "DOCS")
              '())
(check-expect (find-all-dirs (dir-dirs TS) "Docs")
              (list (list "Libs" "Docs")))
(check-expect (find-all-dirs (dir-dirs TS) "part3")
              (list (list "Text" "part3")))
(check-expect (find-all-dirs (dir-dirs TS) "read!")
              (list (list "Libs" "Docs" "read!")))

; [List-of File] String -> [List-of String]
; Return a list of all filenames of the files in lof
; that match the filename f.
(define (find-all-files lof f)
  (cond
    [(empty? lof)
     '()]
    [else
     (local ((define FIRST-NAME (file-name (first lof))))
       ; – IN –
       (cond
         [(string=? FIRST-NAME f)
          (cons FIRST-NAME
                (find-all-files (rest lof) f))]
         [else
          (find-all-files (rest lof) f)]))]))           

(check-expect (find-all-files (dir-files TEXT) "part4")
              '())
(check-expect (find-all-files (dir-files TEXT) "part3")
              (list "part3"))
(check-expect (find-all-files (dir-files TS) "read!")
              (list "read!"))
