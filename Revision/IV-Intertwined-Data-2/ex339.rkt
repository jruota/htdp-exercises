;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex339) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require htdp/dir)

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define TS (create-dir "TS"))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Dir String -> Boolean
; Is file f in the directory dir?
(define (find? dir f)
  (local ((define DIRS (dir-dirs dir))
          (define FILES (dir-files dir))

          ; [List-of File] -> Boolean
          ; Is file f in lof?
          (define (file-find? lof)
            (cond
              [(empty? lof) #false]
              [else
               (or (string=? f (file-name (first lof)))
                   (file-find? (rest lof)))]))

          ; [List-of Dir] -> Boolean
          ; Is file f in any of the directories in lod?
          (define (dir-find? lod)
            (cond
              [(empty? lod) #false]
              [else
               (or (find? (first lod) f)
                   (dir-find? (rest lod)))])))
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

; Dir String -> Boolean
; Is file f in the directory dir?
(define (find?.v2 dir f)
  (local ((define DIRS (dir-dirs dir))
          (define FILES (dir-files dir))

          ; [List-of File] -> Boolean
          ; Is file f in lof?
          (define (file-find?.v2 lof)
            (ormap (lambda (file) (string=? (file-name file) f))
                   lof))

          ; [List-of Dir] -> Boolean
          ; Is file f in any of the directories in lod?
          (define (dir-find?.v2 lod)
            (ormap (lambda (dir0) (find?.v2 dir0 f))
                   lod)))
    ; – IN –
    (or (file-find?.v2 FILES)
        (dir-find?.v2 DIRS))))

(check-expect (find?.v2 (make-dir "Empty" '() '()) "file-name")
              #false)
(check-expect (find?.v2 TS "read!")
              #true)
(check-expect (find?.v2 TS "part3")
              #true)
(check-expect (find?.v2 TS "draw")
              #true)
(check-expect (find?.v2 TS "no-such-file")
              #false)
