;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex339) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; NOTE +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

; According to the design recipe for intertwined data, there should be four
; functions (one for File, [List-of File], Dir and [List-of Dir] respectively).
; The File function would only extract the file name, which is overkill for a
; standalone function and can be easily achieved in the [List-of File] function.

; END NOTE +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

; Dir String -> Boolean
; Does file occur in the directory tree dir?
(define (find? dir file)
  (local (; [List-of File] -> Boolean
          ; Is file in the list of
          ; files lof?
          (define (file-find? lof)
;            (cond
;              [(empty? lof) #false]
;              [else
;               (if (string=? (file-name (first lof)) file)
;                   #true
;                   (file-find? (rest lof)))]))
            (ormap (lambda (x) (string=? (file-name x) file)) lof))

          ; [List-of Dir] -> Boolean
          ; Is file in any of the dirs
          ; in lod?
          (define (lod-find? lod)
;            (cond
;              [(empty? lod) #false]
;              [else
;               (or
;                (dir-find? (first lod))
;                (lod-find? (rest lod)))]))
            (ormap (lambda (x) (dir-find? x)) lod))

          ; Dir -> Boolean
          ; Is file in the directory d?
          (define (dir-find? d)
            (or (file-find? (dir-files d))
                (lod-find? (dir-dirs d)))))
    ; – IN –
    (dir-find? dir)))

(check-expect (find? TS "read!")
              #true)
(check-expect (find? TS "part4")
              #false)
(check-expect (find? TS "hang")
              #true)

; Dir String -> Boolean
; Does file occur in the directory tree dir?
(define (find?.v2 dir file)
  (or (ormap (lambda (x) (string=? (file-name x) file)) (dir-files dir))
      (ormap (lambda (x) (find?.v2 x file)) (dir-dirs dir))))

(check-expect (find?.v2 TS "read!")
              #true)
(check-expect (find?.v2 TS "part4")
              #false)
(check-expect (find?.v2 TS "hang")
              #true)