;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex344) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require htdp/dir)

; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A Path is [List-of String].
; Interpretation:
;     Directions into a directory tree.

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define TS (create-dir "TS"))
(define TS-2 (create-dir "TS-2"))
(define EMPTY (create-dir "EMPTY"))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Dir String -> [List-of Path]
; Return all paths to the file or directory fod
; in d.
(define (find-all d fod)
  (local ((define ALL-PATHS (ls-R d))

          ; Path -> Boolean
          ; Does path p lead to the file or
          ; directory fod?
          (define (path-to-fod? p)
            (local (; NE-Path -> Boolean
                    ; Does the non-empty path nep lead
                    ; to the file or directory fod?
                    (define (nepath-to-fod? nep)
                      (local ((define REST (rest nep)))
                        ; – IN –
                        (cond
                          [(empty? (rest nep))
                           (string=? (first nep) fod)]
                          [else
                           (nepath-to-fod? (rest nep))]))))
              ; – IN –
              (cond
                [(empty? p) #false]
                [else (nepath-to-fod? p)]))))
    ; – IN –
    (cond
      [(empty? ALL-PATHS) '()]
      [else
       (filter path-to-fod? ALL-PATHS)])))

(check-expect (find-all EMPTY "read!")
              '())
(check-expect (find-all TS "no-such-file")
              '())
(check-expect (find-all TS "draw")
              (list (list "TS" "Libs" "Code" "draw")))
(check-expect (find-all TS "Code")
              (list (list "TS" "Libs" "Code")))
(check-expect (find-all TS "read!")
              (list (list "TS" "read!")
                    (list "TS" "Libs" "Docs" "read!")))

; NOTE: slightly different ordering in the following test
;       compared to the one not using ls-R
(check-expect (find-all TS-2 "Text")
              (list (list "TS-2" "Text")
                    (list "TS-2" "Libs" "Text")
                    (list "TS-2" "Text" "Text")))
(check-expect (find-all TS-2 "part2")
              (list (list "TS-2" "Libs" "Text" "part2")
                    (list "TS-2" "Text" "part2")
                    (list "TS-2" "Text" "Text" "part2")))

; from ex343.rtk ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
