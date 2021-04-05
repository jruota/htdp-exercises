;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex341) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require htdp/dir)

; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; (define-struct file [name size content])
; A File is a structure: 
;   (make-file String N String)

; (define-struct dir [name dirs files])
; A Dir is a structure: 
;   (make-dir.v3 String [List-of Dir] [List-of File])

; An N is one of:
; – 0
; – (add1 N)
; Interpretation:
;     The counting numbers,
;     including 0.

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define TS
  (create-dir
   (string-append "/home/jruota/learning/cs/teach-yourself-cs/"
                  "01-HtDP/htdp-exercises/IV-Intertwined-Data/TS")))

(define DIR-SIZE 1)

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; NOTE +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

; According to the design recipe for intertwined data, there should be four
; functions (one for File, [List-of File], Dir and [List-of Dir] respectively).
; The File function would only extract the file size, which is overkill for a
; standalone function and can be easily achieved in the [List-of File] function.

; END NOTE +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

; Dir -> N
; Compute the total size of all the
; files in the entire directory tree.
(define (du dir)
  (local (; [List-of File] -> N
          ; Return the total size of
          ; all the files in lof.
          (define (lof-du lof)
;            (cond
;              [(empty? lof) 0]
;              [else
;               (+ (file-size (first lof))
;                  (lof-du (rest lof)))]))
            (foldl + 0 (map file-size lof)))

          ; [List-of Dir] -> N
          ; Return the total size of all
          ; the directories in lod.
          (define (lod-du lod)
;            (cond
;              [(empty? lod) 0]
;              [else
;               (+ (du (first lod))
;                  (lod-du (rest lod)))]))
            (foldl + 0 (map du lod)))

          ; Dir -> N
          ; Return the total size of all
          ; files and directories in d.
          (define (dir-du d)
            (+ DIR-SIZE
               (lof-du (dir-files d))
               (lod-du (dir-dirs d)))))
    ; – IN –
    (dir-du dir)))

(check-expect (du (make-dir "empty" '() '()))
              1)
(check-expect (du TS)
              212)

; Dir -> N
; Compute the total size of all the
; files in the entire directory tree.
(define (du.v2 dir)
  (+ DIR-SIZE
     (foldl + 0 (map file-size (dir-files dir)))
     (foldl + 0 (map du.v2 (dir-dirs dir)))))

(check-expect (du.v2 (make-dir "empty" '() '()))
              1)
(check-expect (du.v2 TS)
              212)