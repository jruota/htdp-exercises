;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex341) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require htdp/dir)

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define TS (create-dir "TS"))
(define TS-2 (create-dir "TS-2"))

(define DIR-STORAGE-UNIT 1)

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Dir -> N
; Compute the total size of all the files in
; the entire directory tree dir.
(define (du dir)
  (local (; [List-of Dir] -> N
          ; Return the total size of all the files
          ; in all the directory trees in lod.
          (define (lod-du lod)
            (foldl (lambda (d acc) (+ (du d) acc))
                   0
                   lod))

          ; [List-of File] -> N
          ; Return the total size of all the files in lof.
          (define (lof-du lof)
            (foldl (lambda (f acc) (+ (file-size f) acc))
                   0
                   lof)))
    ; – IN –
    (+ DIR-STORAGE-UNIT
       (lod-du (dir-dirs dir))
       (lof-du (dir-files dir)))))

(check-expect (du (make-dir "Empty" '() '()))
              1)
(check-expect (du TS)
              212)
(check-expect (du TS-2)
              509)
