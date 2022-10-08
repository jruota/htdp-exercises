;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex342) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require htdp/dir)

; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A Path is [List-of String].
; Interpretation:
;     Directions into a directory tree.

; A PathOrFalse is one of:
; – Path
; – #false

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define TS (create-dir "TS"))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Dir String -> PathOrFalse
; If there is a file or directory fod in the directory tree d,
; return the path to it. Otherwise, return #false.
(define (find d fod)
  (local (; [List-of Dir] String -> PathOrFalse
          ; If the file or directory fod is any or in any of
          ; the directories in lod, return the path to it.
          ; Otherwise, return #false.
          (define (dir-find lod)
            (cond
              [(empty? lod) #false]
              [else
               (local ((define FIRST (first lod))
                       (define FIRST-NAME (dir-name FIRST)))
                 ; – IN –
                 (cond
                   [(string=? FIRST-NAME fod)
                    (list FIRST-NAME)]
                   [(find? FIRST fod)
                    (find FIRST fod)]
                   [else
                    (dir-find (rest lod))]))]))

          ; [List-of File] String -> PathOrFalse
          ; If the file fod is among the list of files lof,
          ; return the path to it. Otherwise, return #false.
          (define (file-find lof)
            (cond
              [(empty? lof) #false]
              [else
               (local ((define FIRST-NAME (file-name (first lof))))
                 ; – IN –
                 (cond
                   [(string=? FIRST-NAME fod)
                    (list FIRST-NAME)]
                   [else
                    (file-find (rest lof))]))]))

          (define FILES (file-find (dir-files d)))
          (define DIRS (dir-find (dir-dirs d))))
    ; – IN –
    (cond
      [(cons? FILES)
       (cons (dir-name d) FILES)]
      [(cons? DIRS)
       (cons (dir-name d) DIRS)]
      [else #false])))

(check-expect (find (make-dir "Empty" '() '()) "no-such-file")
              #false)
(check-expect (find TS "no-such-file")
              #false)
(check-expect (find TS "read!")
              (list "TS" "read!"))
(check-expect (find TS "Libs")
              (list "TS" "Libs"))
(check-expect (find TS "part1")
              (list "TS" "Text" "part1"))
(check-expect (find TS "Code")
              (list "TS" "Libs" "Code"))

; Dir String -> PathOrFalse
; If there is a file or directory fod in the directory tree d,
; return the path to it. Otherwise, return #false.
(define (find.v2 d fod)
  (local (; [List-of Dir] String -> PathOrFalse
          ; If the file or directory fod is any or in any of
          ; the directories in lod, return the path to it.
          ; Otherwise, return #false.
          (define (dir-find lod)
            (cond
              [(empty? lod) #false]
              [else
               (local ((define FIRST (first lod))
                       (define FIRST-NAME (dir-name FIRST))
                       (define FURTHER (find.v2 FIRST fod)))
                 ; – IN –
                 (cond
                   [(string=? FIRST-NAME fod)
                    (list FIRST-NAME)]
                   [(not (boolean? FURTHER))
                    FURTHER]
                   [else
                    (dir-find (rest lod))]))]))

          ; [List-of File] String -> PathOrFalse
          ; If the file fod is among the list of files lof,
          ; return the path to it. Otherwise, return #false.
          (define (file-find lof)
            (cond
              [(empty? lof) #false]
              [else
               (local ((define FIRST-NAME (file-name (first lof))))
                 ; – IN –
                 (cond
                   [(string=? FIRST-NAME fod)
                    (list FIRST-NAME)]
                   [else
                    (file-find (rest lof))]))]))

          (define FILES (file-find (dir-files d)))
          (define DIRS (dir-find (dir-dirs d))))
    ; – IN –
    (cond
      [(cons? FILES)
       (cons (dir-name d) FILES)]
      [(cons? DIRS)
       (cons (dir-name d) DIRS)]
      [else #false])))

(check-expect (find.v2 (make-dir "Empty" '() '()) "no-such-file")
              #false)
(check-expect (find.v2 TS "no-such-file")
              #false)
(check-expect (find.v2 TS "read!")
              (list "TS" "read!"))
(check-expect (find.v2 TS "Libs")
              (list "TS" "Libs"))
(check-expect (find.v2 TS "part1")
              (list "TS" "Text" "part1"))
(check-expect (find.v2 TS "Code")
              (list "TS" "Libs" "Code"))

; [List-of File] String -> PathOrFalse
; If the file fod is among the list of files lof,
; return the path to it. Otherwise, return #false.
(define (file-find lof fod)
  (cond
    [(empty? lof) #false]
    [else
     (cond
       [(string=? (file-name (first lof)) fod)
        (list (file-name (first lof)))]
       [else
        (file-find (rest lof) fod)])]))

(define TEXT (second (dir-dirs TS)))
(check-expect (file-find (dir-files TEXT) "no-such-file")
              #false)
(check-expect (file-find '() "part1")
              #false)
(check-expect (file-find (dir-files TEXT) "part2")
              (list "part2"))

; [List-of Dir] String -> PathOrFalse
; If the file or directory fod is any or in any of
; the directories in lod, return the path to it.
; Otherwise, return #false.
(define (dir-find lod fod)
  (cond
    [(empty? lod) #false]
    [else
     (cond
       [(string=? (dir-name (first lod)) fod)
        (list (dir-name (first lod)))]
       [(find? (first lod) fod)
        (find (first lod) fod)]
       [else
        (dir-find (rest lod) fod)])]))

(define DIRECTORIES (dir-dirs TS))
(check-expect (dir-find DIRECTORIES "no-such-file")
              #false)
(check-expect (dir-find '() "part2")
              #false)
(check-expect (dir-find DIRECTORIES "part2")
              (list "Text" "part2"))
(check-expect (dir-find DIRECTORIES "Libs")
              (list "Libs"))
(check-expect (dir-find DIRECTORIES "Code")
              (list "Libs" "Code"))
(check-expect (dir-find DIRECTORIES "draw")
              (list "Libs" "Code" "draw"))

; [List-of Dir] String -> PathOrFalse
; If the file or directory fod is any or in any of
; the directories in lod, return the path to it.
; Otherwise, return #false.
(define (dir-find.v2 lod fod)
  (cond
    [(empty? lod) #false]
    [else
     (local ((define FURTHER (find (first lod) fod)))
       ; – IN –
       (cond
         [(string=? (dir-name (first lod)) fod)
          (list (dir-name (first lod)))]
;       [(find? (first lod) fod)
;        (find (first lod) fod)]
         [(not (false? FURTHER))
          FURTHER]
         [else
          (dir-find.v2 (rest lod) fod)]))]))

(check-expect (dir-find.v2 DIRECTORIES "no-such-file")
              #false)
(check-expect (dir-find.v2 '() "part2")
              #false)
(check-expect (dir-find.v2 DIRECTORIES "part2")
              (list "Text" "part2"))
(check-expect (dir-find.v2 DIRECTORIES "Libs")
              (list "Libs"))
(check-expect (dir-find.v2 DIRECTORIES "Code")
              (list "Libs" "Code"))
(check-expect (dir-find.v2 DIRECTORIES "draw")
              (list "Libs" "Code" "draw"))

; from ex339.rkt (slightly changed) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Dir String -> Boolean
; Is the file or directory fod in the directory dir?
(define (find? dir fod)
  (local ((define DIRS (dir-dirs dir))
          (define FILES (dir-files dir))

          ; [List-of File] -> Boolean
          ; Is file f in lof?
          (define (file-find? lof)
            (ormap (lambda (file) (string=? (file-name file) fod))
                   lof))

          ; [List-of Dir] -> Boolean
          ; Is file f in any of the directories in lod?
          (define (dir-find? lod)
            (or
             (ormap (lambda (dir0) (string=? (dir-name dir0) fod))
                    lod)
             (ormap (lambda (dir1) (find? dir1 fod))
                    lod))))
    ; – IN –
    (or (file-find? FILES)
        (dir-find? DIRS))))

(check-expect (find? (make-dir "Empty" '() '()) "file-name")
              #false)
(check-expect (find? TS "read!")
              #true)
(check-expect (find? TS "part3")
              #true)
(check-expect (find? TS "draw")
              #true)
(check-expect (find? TS "no-such-file")
              #false)
(check-expect (find? TS "Code")
              #true)
