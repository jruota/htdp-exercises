;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex342) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

; Dir String -> MaybePath
; Produce a path to file f in dir.
; If no such file exists, return #false.
(define (find dir f)
  (local (; [List-of File] -> MaybePath
          ; If file f is in lof, return its name.
          (define (find-file lof)
            (cond
              [(empty? lof) #false]
              [else
               (if (string=? (file-name (first lof)) f)
                   (cons f '())
                   (find-file (rest lof)))]))

          ; [List-of Dir] -> MaybePath
          ; If file f is in any of the directories in lod,
          ; return its Path.
          (define (find-in-dirs lod)
            (cond
              [(empty? lod) #false]
              [else
               (local ((define here (find-file (dir-files (first lod))))
                       (define there (find-in-dirs (dir-dirs (first lod)))))
                 ; – IN –
                 (cond
                   [(not (boolean? here))
                    (cons (dir-name (first lod)) here)]
                   [(not (boolean? there))
                    (cons (dir-name (first lod)) there)]
                   [else
                    (find-in-dirs (rest lod))]))]))

          ; Dir -> MaybePath
          ; Return the path to file f if it is in dir0.
          (define (find-in-dir dir0)
            (local ((define here (find-file (dir-files dir0)))
                    (define there (find-in-dirs (dir-dirs dir0))))
              ; – IN –
              (cond
                [(not (boolean? here))
                 (cons (dir-name dir0) here)]
                [(not (boolean? there))
                 (cons (dir-name dir0) there)]
                [else #false]))))
              
    ; – IN –
    (find-in-dir dir)))

(check-expect (find TS "no-such-file") #false)
(check-expect (find TS "read!") (list "TS" "read!"))
(check-expect (find TS "part1") (list "TS" "Text" "part1"))
(check-expect (find TS "hang") (list "TS" "Libs" "Code" "hang"))
