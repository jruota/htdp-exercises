;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex340) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require htdp/dir)

; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; (define-struct file [name size content])
; A File is a structure: 
;   (make-file String N String)

; (define-struct dir [name dirs files])
; A Dir is a structure: 
;   (make-dir.v3 String [List-of Dir] [List-of File])

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define TS
  (create-dir
   (string-append "/home/jruota/learning/cs/teach-yourself-cs/"
                  "01-HtDP/htdp-exercises/IV-Intertwined-Data/TS")))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; NOTE +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

; According to the design recipe for intertwined data, there should be four
; functions (one for File, [List-of File], Dir and [List-of Dir] respectively).
; The File function would only extract the file name, which is overkill for a
; standalone function and can be easily achieved in the [List-of File] function.

; END NOTE +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

; Dir -> [List-of String]
; List the names of all files and directories in dir.
(define (ls dir)
  (local (; [List-of File] -> [List-of String]
          ; Return all the names of the files
          ; in lof.
          (define (lof-ls lof)
;            (cond
;              [(empty? lof) '()]
;              [else
;               (cons (file-name (first lof))
;                     (lof-ls (rest lof)))]))
            (map file-name lof))

          ; [List-of Dir] -> [List-of String]
          ; Return all the names of the
          ; directories in lod.
          (define (lod-ls lod)
;            (cond
;              [(empty? lod) '()]
;              [else
;               (cons (dir-name (first lod))
;                     (lod-ls (rest lod)))]))
            (map dir-name lod))

          ; Dir -> [List-of String]
          ; List all the names of the
          ; files and directories in d,
          ; sorted alphabetically.
          (define (dir-ls d)
            (sort
             (append (lof-ls (dir-files d))
                     (lod-ls (dir-dirs d)))
             string<?)))
    ; – IN –
    (dir-ls dir)))

(check-expect (ls (make-dir "empty" '() '()))
              '())
(check-expect (ls TS)
              (sort (list "Libs" "read!" "Text") string<?))

; Dir -> [List-of String]
; List the names of all files and directories in dir.
(define (ls.v2 dir)
  (sort
   (append (map file-name (dir-files dir))
           (map dir-name (dir-dirs dir)))
   string<?))

(check-expect (ls.v2 (make-dir "empty" '() '()))
              '())
(check-expect (ls.v2 TS)
              (sort (list "Libs" "read!" "Text") string<?))