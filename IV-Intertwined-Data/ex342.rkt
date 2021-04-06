;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex342) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require htdp/dir)

; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; (define-struct file [name size content])
; A File is a structure: 
;   (make-file String N String)

; (define-struct dir [name dirs files])
; A Dir is a structure: 
;   (make-dir String [List-of Dir] [List-of File])

; A PathOrFalse is one of:
; – #false
; – [List-of String]
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

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Dir String -> PathOrFalse
; If file f is somewhere in directory dir,
; return the path to file f, #false otherwise.
(define (find dir f)
  (local (; [List-of File] -> StringOrFalse
          ; Return the name of file f
          ; if it is in lof,
          ; #false otherwise.
          (define (lof-find lof)
            (cond
              [(empty? lof) #false]
              [else
               (if (string=? (file-name (first lof)) f)
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
               (if (find? (first lod) f)
                   (dir-find (first lod))
                   (lod-find (rest lod)))]))

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
                       f)]
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

; from ex399.rkt +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

; Dir String -> Boolean
; Does file occur in the directory tree dir?
(define (find? dir file)
  (or (ormap (lambda (x) (string=? (file-name x) file)) (dir-files dir))
      (ormap (lambda (x) (find? x file)) (dir-dirs dir))))

(check-expect (find? TS "read!")
              #true)
(check-expect (find? TS "part4")
              #false)
(check-expect (find? TS "hang")
              #true)